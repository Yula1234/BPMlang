#pragma once

namespace iropt {

struct FunctionInfo {
    GString name;
    size_t      start;  
    size_t      end;    
};

inline bool is_function_label_name(const GString& name) {
    if (name.empty()) return false;
    if (name[0] == '.') return false; 
    if (name.size() >= 4) {
        const GString suffix = "@ret";
        if (name.compare(name.size() - suffix.size(), suffix.size(), suffix) == 0)
            return false; 
    }
    return true;
}

inline GVector<FunctionInfo> collect_functions(const IRProgram& prog) {
    const auto& instrs = prog.instrs;
    GVector<std::pair<GString, size_t>> func_labels;

    for (size_t i = 0; i < instrs.size(); ++i) {
        const IRInstr& ins = instrs[i];
        if (ins.op == IROp::Label && ins.a.kind == OperandKind::Label) {
            const GString& lbl = ins.a.name;
            if (is_function_label_name(lbl)) {
                func_labels.emplace_back(lbl, i);
            }
        }
    }

    GVector<FunctionInfo> functions;
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

inline GSet<GString>
compute_reachable_functions(const IRProgram& prog,
                            const GVector<FunctionInfo>& funcs)
{
    GSet<GString> reachable;
    GVector<GString>        worklist;

    GMap<GString, const FunctionInfo*> name_to_func;
    name_to_func.reserve(funcs.size());
    for (const auto& f : funcs) {
        name_to_func[f.name] = &f;
    }

    auto mark = [&](const GString& fname) {
        auto itf = name_to_func.find(fname);
        if (itf == name_to_func.end()) return;
        if (reachable.insert(fname).second) {
            worklist.push_back(fname);
        }
    };

    mark("main");

    while (!worklist.empty()) {
        GString cur = worklist.back();
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
    GVector<FunctionInfo> funcs = collect_functions(prog);
    if (funcs.empty()) return;

    GSet<GString> reachable =
        compute_reachable_functions(prog, funcs);

    const size_t N = prog.instrs.size();
    GVector<bool> kill(N, false);

    for (const auto& f : funcs) {
        if (f.name == "main") continue;
        if (reachable.find(f.name) == reachable.end()) {
            for (size_t i = f.start; i < f.end; ++i) {
                kill[i] = true;
            }
        }
    }

    GVector<IRInstr> new_instrs;
    new_instrs.reserve(N);
    for (size_t i = 0; i < N; ++i) {
        if (!kill[i]) {
            new_instrs.push_back(std::move(prog.instrs[i]));
        }
    }

    prog.instrs.swap(new_instrs);
}

inline bool instr_reads_reg(const IRInstr& ins, Reg r) {
    auto check = [&](const Operand& op) {
        if (op.kind == OperandKind::Reg && op.reg == r) return true;
        if (op.kind == OperandKind::Mem && op.mem.hasBase && op.mem.base == r) return true;
        return false;
    };
    return check(ins.a) || check(ins.b) || check(ins.c);
}


inline void peephole_noops(IRProgram& prog) {
    GVector<IRInstr> out;
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

inline void optimize_ir(IRProgram& prog) {
    remove_unused_functions(prog);
    peephole_noops(prog);
}

} // namespace iropt