#pragma once
#include "ir.hpp"

#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <string>
#include <set>
#include <algorithm>

namespace iropt {

struct FunctionInfo {
    std::string name;
    size_t      start;  
    size_t      end;    
};

inline bool is_function_label_name(const std::string& name) {
    if (name.empty()) return false;
    if (name[0] == '.') return false; 
    if (name.size() >= 4) {
        const std::string suffix = "@ret";
        if (name.compare(name.size() - suffix.size(), suffix.size(), suffix) == 0)
            return false; 
    }
    return true;
}

inline std::vector<FunctionInfo> collect_functions(const IRProgram& prog) {
    const auto& instrs = prog.instrs;
    std::vector<std::pair<std::string, size_t>> func_labels;

    for (size_t i = 0; i < instrs.size(); ++i) {
        const IRInstr& ins = instrs[i];
        if (ins.op == IROp::Label && ins.a.kind == OperandKind::Label) {
            const std::string& lbl = ins.a.name;
            if (is_function_label_name(lbl)) {
                func_labels.emplace_back(lbl, i);
            }
        }
    }

    std::vector<FunctionInfo> functions;
    if (func_labels.empty()) return functions;

    const size_t N = instrs.size();
    functions.reserve(func_labels.size());
    for (size_t i = 0; i < func_labels.size(); ++i) {
        const auto& cur  = func_labels[i];
        size_t start_idx = cur.second;
        size_t end_idx   = (i + 1 < func_labels.size() ? func_labels[i + 1].second : N);
        functions.push_back(FunctionInfo{ cur.first, start_idx, end_idx });
    }
    return functions;
}

inline std::unordered_set<std::string>
compute_reachable_functions(const IRProgram& prog,
                            const std::vector<FunctionInfo>& funcs)
{
    std::unordered_set<std::string> reachable;
    std::vector<std::string>        worklist;

    std::unordered_map<std::string, const FunctionInfo*> name_to_func;
    name_to_func.reserve(funcs.size());
    for (const auto& f : funcs) {
        name_to_func[f.name] = &f;
    }

    auto mark = [&](const std::string& fname) {
        auto itf = name_to_func.find(fname);
        if (itf == name_to_func.end()) return;
        if (reachable.insert(fname).second) {
            worklist.push_back(fname);
        }
    };

    
    mark("main");

    while (!worklist.empty()) {
        std::string cur = worklist.back();
        worklist.pop_back();

        auto itf = name_to_func.find(cur);
        if (itf == name_to_func.end()) continue;
        const FunctionInfo& fi = *itf->second;

        for (size_t i = fi.start; i < fi.end; ++i) {
            const IRInstr& ins = prog.instrs[i];

            auto scan_operand = [&](const Operand& op) {
                if (op.kind == OperandKind::Symbol) {
                    mark(op.name);
                }
            };
            scan_operand(ins.a);
            scan_operand(ins.b);
            scan_operand(ins.c);
        }
    }

    return reachable;
}

inline void remove_unused_functions(IRProgram& prog) {
    std::vector<FunctionInfo> funcs = collect_functions(prog);
    if (funcs.empty()) return;

    std::unordered_set<std::string> reachable =
        compute_reachable_functions(prog, funcs);

    const size_t N = prog.instrs.size();
    std::vector<bool> kill(N, false);

    for (const auto& f : funcs) {
        if (f.name == "main") continue;
        if (reachable.find(f.name) == reachable.end()) {
            for (size_t i = f.start; i < f.end; ++i) {
                kill[i] = true;
            }
        }
    }

    std::vector<IRInstr> new_instrs;
    new_instrs.reserve(N);
    for (size_t i = 0; i < N; ++i) {
        if (!kill[i]) {
            new_instrs.push_back(std::move(prog.instrs[i]));
        }
    }

    prog.instrs.swap(new_instrs);
}


inline void peephole_noops(IRProgram& prog) {
    std::vector<IRInstr> out;
    out.reserve(prog.instrs.size());

    const auto& in = prog.instrs;
    const size_t N = in.size();

    for (size_t i = 0; i < N; ++i) {
        const IRInstr& ins = in[i];

        if (ins.op == IROp::Mov &&
            ins.a.kind == OperandKind::Reg &&
            ins.b.kind == OperandKind::Reg &&
            ins.a.reg  == ins.b.reg)
        {
            continue;
        }

        if (ins.op == IROp::Push && i + 1 < N) {
            const IRInstr& next = in[i+1];

            if (ins.a.kind == OperandKind::Reg &&
                next.op == IROp::Pop &&
                next.a.kind == OperandKind::Reg &&
                next.a.reg == ins.a.reg)
            {
                i += 1;
                continue;
            }

            if (next.op == IROp::Pop &&
                next.a.kind == OperandKind::Reg)
            {
                Operand dst = next.a;
                Operand src = ins.a;
                out.emplace_back(IROp::Mov, dst, src);
                i += 1;
                continue;
            }
        }

        out.push_back(ins);
    }

    prog.instrs.swap(out);
}

struct BasicBlock {
    size_t              index;
    size_t              start;
    size_t              end;
    std::string         label;
    std::vector<size_t> succs;
};

inline bool is_label_instr(const IRInstr& ins) {
    return ins.op == IROp::Label;
}

inline bool is_uncond_jump(IROp op) {
    return op == IROp::Jmp;
}

inline bool is_cond_jump(IROp op) {
    return op == IROp::Jz  || op == IROp::Jnz ||
           op == IROp::Je  || op == IROp::Jne;
}

inline bool is_return_instr(IROp op) {
    return op == IROp::Ret;
}

inline bool is_block_terminator(const IRInstr& ins) {
    return is_uncond_jump(ins.op) ||
           is_cond_jump(ins.op)   ||
           is_return_instr(ins.op);
}

inline void build_basic_blocks_and_cfg(
    const IRProgram& prog,
    std::vector<BasicBlock>& blocks,
    std::unordered_map<std::string, size_t>& label_to_block)
{
    const auto& instrs = prog.instrs;
    const size_t N = instrs.size();
    blocks.clear();
    label_to_block.clear();
    if (N == 0) return;

    
    std::vector<bool> is_leader(N, false);
    is_leader[0] = true;

    for (size_t i = 0; i < N; ++i) {
        if (is_label_instr(instrs[i])) {
            is_leader[i] = true;
        }
    }

    for (size_t i = 0; i < N; ++i) {
        if (is_block_terminator(instrs[i]) && i + 1 < N) {
            is_leader[i + 1] = true;
        }
    }

    
    size_t cur = 0;
    while (cur < N) {
        if (!is_leader[cur]) {
            ++cur;
            continue;
        }
        size_t start = cur;
        size_t end   = cur + 1;
        while (end < N && !is_leader[end]) ++end;

        BasicBlock bb;
        bb.index = blocks.size();
        bb.start = start;
        bb.end   = end;
        bb.label.clear();
        blocks.push_back(bb);

        cur = end;
    }

    
    for (auto& bb : blocks) {
        if (bb.start < N) {
            const IRInstr& first = instrs[bb.start];
            if (first.op == IROp::Label && first.a.kind == OperandKind::Label) {
                bb.label = first.a.name;
                label_to_block[bb.label] = bb.index;
            }
        }
    }

    
    for (auto& bb : blocks) {
        bb.succs.clear();
        if (bb.end == 0) continue;
        const IRInstr& last = instrs[bb.end - 1];

        if (is_uncond_jump(last.op)) {
            if (last.a.kind == OperandKind::Label) {
                auto it = label_to_block.find(last.a.name);
                if (it != label_to_block.end()) {
                    bb.succs.push_back(it->second);
                }
            }
        } else if (is_cond_jump(last.op)) {
            if (last.a.kind == OperandKind::Label) {
                auto it = label_to_block.find(last.a.name);
                if (it != label_to_block.end()) {
                    bb.succs.push_back(it->second);
                }
            }
            if (bb.index + 1 < blocks.size()) {
                bb.succs.push_back(bb.index + 1);
            }
        } else if (is_return_instr(last.op)) {
            
        } else {
            if (bb.index + 1 < blocks.size()) {
                bb.succs.push_back(bb.index + 1);
            }
        }
    }
}

inline void read_operand_regs(const Operand& op, std::set<Reg>& out) {
    if (op.kind == OperandKind::Reg) {
        out.insert(op.reg);
    } else if (op.kind == OperandKind::Mem) {
        if (op.mem.hasBase)
            out.insert(op.mem.base);
    }
}

inline void get_read_regs(const IRInstr& ins, std::set<Reg>& out) {
    switch (ins.op) {
    case IROp::Mov:
        
        
        read_operand_regs(ins.a, out);
        read_operand_regs(ins.b, out);
        break;

    case IROp::Add:
    case IROp::Sub:
    case IROp::Xor:
    case IROp::Shl:
    case IROp::Shr:
    case IROp::Cmp:
    case IROp::Test:
        read_operand_regs(ins.a, out);
        read_operand_regs(ins.b, out);
        break;

    case IROp::Store8:
    case IROp::Store16:
        
        read_operand_regs(ins.a, out);
        read_operand_regs(ins.b, out);
        break;

    case IROp::Load8:
    case IROp::Load16:
        
        read_operand_regs(ins.b, out);
        break;

    case IROp::Call:
    case IROp::Push:
    case IROp::Pop:
        
        read_operand_regs(ins.a, out);
        break;

    case IROp::Ret:
        
        out.insert(Reg::EAX);
        break;

    default:
        
        read_operand_regs(ins.a, out);
        read_operand_regs(ins.b, out);
        read_operand_regs(ins.c, out);
        break;
    }
}

inline void get_written_regs(const IRInstr& ins, std::set<Reg>& out) {
    auto write_reg = [&](const Operand& o) {
        if (o.kind == OperandKind::Reg) out.insert(o.reg);
    };

    switch (ins.op) {
    case IROp::Mov:
    case IROp::Add:
    case IROp::Sub:
    case IROp::Xor:
    case IROp::Shl:
    case IROp::Shr:
    case IROp::Inc:
        write_reg(ins.a);
        break;

    case IROp::Load8:
    case IROp::Load16:
        write_reg(ins.a);
        break;

    case IROp::Pop:
        write_reg(ins.a);
        break;

    default:
        break;
    }
}

inline bool is_removable_mov_to_reg(const IRInstr& ins) {
    if (ins.op != IROp::Mov) return false;
    if (ins.a.kind != OperandKind::Reg) return false;
    if (ins.b.kind == OperandKind::Mem) return false; 
    return true;
}

inline void dce_regs_in_block(std::vector<IRInstr>& block, const std::set<Reg>& live_out_end) {
    std::vector<IRInstr> new_block;
    new_block.reserve(block.size());

    std::set<Reg> live = live_out_end;

    
    for (int i = static_cast<int>(block.size()) - 1; i >= 0; --i) {
        const IRInstr& ins = block[i];

        std::set<Reg> readR;
        std::set<Reg> writeR;
        get_read_regs(ins, readR);
        get_written_regs(ins, writeR);

        bool removable = is_removable_mov_to_reg(ins);
        bool used      = true;

        if (removable) {
            Reg dest = ins.a.reg;
            if (live.count(dest) == 0) {
                used = false;
            }
        }

        if (used) {
            new_block.push_back(ins);
            
            for (Reg r : writeR) {
                live.erase(r);
            }
            for (Reg r : readR) {
                live.insert(r);
            }
        } else {
            
            
        }
    }

    std::reverse(new_block.begin(), new_block.end());
    block.swap(new_block);
}

inline void dce_regs(IRProgram& prog) {
    std::vector<BasicBlock> blocks;
    std::unordered_map<std::string, size_t> label_to_block;
    build_basic_blocks_and_cfg(prog, blocks, label_to_block);
    if (blocks.empty()) return;

    const size_t B = blocks.size();

    
    std::vector<std::set<Reg>> def(B), use(B);
    for (const auto& bb : blocks) {
        std::set<Reg> d, u;
        for (size_t i = bb.start; i < bb.end; ++i) {
            const IRInstr& ins = prog.instrs[i];
            std::set<Reg> readR, writeR;
            get_read_regs(ins, readR);
            get_written_regs(ins, writeR);

            for (Reg r : readR) {
                if (d.count(r) == 0) u.insert(r);
            }
            for (Reg r : writeR) {
                d.insert(r);
            }
        }
        def[bb.index] = std::move(d);
        use[bb.index] = std::move(u);
    }

    
    std::vector<std::set<Reg>> live_in(B), live_out(B);
    bool changed = true;
    while (changed) {
        changed = false;
        for (int bi = static_cast<int>(B) - 1; bi >= 0; --bi) {
            BasicBlock& bb = blocks[bi];

            std::set<Reg> new_out;
            for (size_t s : bb.succs) {
                const auto& lin = live_in[s];
                new_out.insert(lin.begin(), lin.end());
            }

            std::set<Reg> new_in = use[bi];
            
            for (Reg r : live_out[bi]) {
                if (def[bi].count(r) == 0) {
                    new_in.insert(r);
                }
            }

            if (new_out != live_out[bi] || new_in != live_in[bi]) {
                live_out[bi] = std::move(new_out);
                live_in[bi]  = std::move(new_in);
                changed = true;
            }
        }
    }

    
    const auto& in  = prog.instrs;
    const size_t N  = in.size();
    std::vector<IRInstr> out;
    out.reserve(N);

    for (const auto& bb : blocks) {
        std::vector<IRInstr> block_instrs;
        block_instrs.reserve(bb.end - bb.start);
        for (size_t i = bb.start; i < bb.end; ++i) {
            block_instrs.push_back(in[i]);
        }

        dce_regs_in_block(block_instrs, live_out[bb.index]);

        for (auto& ins : block_instrs) {
            out.push_back(std::move(ins));
        }
    }

    prog.instrs.swap(out);
}

inline void optimize_ir(IRProgram& prog) {
    
    remove_unused_functions(prog);

    
    peephole_noops(prog);

    
    dce_regs(prog);
}

} 