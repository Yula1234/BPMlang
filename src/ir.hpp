#pragma once

enum class Reg {
    EAX, EBX, ECX, EDX,
    ESI, EDI,
    EBP, ESP,
    CL,
};

enum class IROp {
    Add,
    Sub,
    Mul,
    IMul,
    Div,
    Xor,
    Mov,
    Store8,
    Store16,
    Load8,    
    Load16,   
    Shl,
    Shr,
    Cmp,
    CMovE,
    CMovNE,
    CMovL,
    CMovG,
    Test,
    Inc,
    Jmp,
    Jz,
    Jnz,
    Je,
    Jne,
    Call,
    Push,
    Pop,
    Ret,
    Label,
    Comment,
    InlineAsm 
};

struct MemRef {
    bool        hasBase   = false;
    Reg         base;
    bool        hasSymbol = false;
    GString symbol;
    int32_t     disp      = 0;

    static MemRef baseDisp(Reg b, int32_t d = 0) {
        MemRef m;
        m.hasBase = true;
        m.base    = b;
        m.disp    = d;
        return m;
    }

    static MemRef sym(const GString& s, int32_t d = 0) {
        MemRef m;
        m.hasSymbol = true;
        m.symbol    = s;
        m.disp      = d;
        return m;
    }

    static MemRef baseSym(Reg b, const GString& s, int32_t d = 0) {
        MemRef m;
        m.hasBase   = true;
        m.base      = b;
        m.hasSymbol = true;
        m.symbol    = s;
        m.disp      = d;
        return m;
    }
};

enum class OperandKind {
    None,
    Reg,
    Imm,
    Mem,
    Label,  
    Symbol  
};

struct Operand {
    OperandKind kind = OperandKind::None;

    Reg         reg;
    int32_t     imm = 0;
    MemRef      mem;
    GString name;

    static Operand none() {
        return Operand{};
    }

    static Operand regOp(Reg r) {
        Operand o;
        o.kind = OperandKind::Reg;
        o.reg  = r;
        return o;
    }

    static Operand immOp(int32_t v) {
        Operand o;
        o.kind = OperandKind::Imm;
        o.imm  = v;
        return o;
    }

    static Operand memOp(const MemRef& m) {
        Operand o;
        o.kind = OperandKind::Mem;
        o.mem  = m;
        return o;
    }

    static Operand labelOp(const GString& n) {
        Operand o;
        o.kind = OperandKind::Label;
        o.name = n;
        return o;
    }

    static Operand symbolOp(const GString& n) {
        Operand o;
        o.kind = OperandKind::Symbol;
        o.name = n;
        return o;
    }
};

struct IRInstr {
    IROp     op;
    Operand  a; 
    Operand  b; 
    Operand  c; 

    IRInstr(IROp _op) : op(_op) {}
    IRInstr(IROp _op, const Operand& _a)
        : op(_op), a(_a) {}
    IRInstr(IROp _op, const Operand& _a, const Operand& _b)
        : op(_op), a(_a), b(_b) {}
    IRInstr(IROp _op, const Operand& _a, const Operand& _b, const Operand& _c)
        : op(_op), a(_a), b(_b), c(_c) {}
};


struct IRStringLiteral {
    GString label; 
    GString value; 
};

struct IRGlobalVar {
    GString name;
};

struct IRProgram {
    GVector<IRInstr>    instrs;
    GVector<GString>   externs;  
    GVector<IRStringLiteral> strings;
    GVector<IRGlobalVar>   globals;  
};


class IRBuilder {
public:
    IRProgram* cur = nullptr;

    explicit IRBuilder(IRProgram* f = nullptr) : cur(f) {}

    void emit(const IRInstr& ins) {
        cur->instrs.push_back(ins);
    }

    void label(const GString& name) {
        emit(IRInstr(IROp::Label, Operand::labelOp(name)));
    }

    void comment(const GString& text) {
        emit(IRInstr(IROp::Comment, Operand::symbolOp(text)));
    }

    void add(const Operand& dst, const Operand& src) {
        emit(IRInstr(IROp::Add, dst, src));
    }

    void sub(const Operand& dst, const Operand& src) {
        emit(IRInstr(IROp::Sub, dst, src));
    }

    void mov(const Operand& dst, const Operand& src) {
        emit(IRInstr(IROp::Mov, dst, src));
    }

    void push(const Operand& x) {
        emit(IRInstr(IROp::Push, x));
    }

    void pop(const Operand& x) {
        emit(IRInstr(IROp::Pop, x));
    }

    void call(const Operand& target) {
        emit(IRInstr(IROp::Call, target));
    }

    void jmp(const Operand& target) {
        emit(IRInstr(IROp::Jmp, target));
    }

    void jz(const Operand& target) {
        emit(IRInstr(IROp::Jz, target));
    }

    void jnz(const Operand& target) {
        emit(IRInstr(IROp::Jnz, target));
    }

    void je(const Operand& target) {
        emit(IRInstr(IROp::Je, target));
    }
    
    void jne(const Operand& target) {
        emit(IRInstr(IROp::Jne, target));
    }

    void ret() {
        emit(IRInstr(IROp::Ret));
    }
};

inline const char* reg_name(Reg r) {
    switch (r) {
    case Reg::EAX: return "eax";
    case Reg::EBX: return "ebx";
    case Reg::ECX: return "ecx";
    case Reg::EDX: return "edx";
    case Reg::ESI: return "esi";
    case Reg::EDI: return "edi";
    case Reg::EBP: return "ebp";
    case Reg::ESP: return "esp";
    case Reg::CL: return "cl";
    }
    return "eax";
}

inline void print_operand(std::ostream& out, const Operand& op) {
    switch (op.kind) {
    case OperandKind::None: break;
    case OperandKind::Reg:
        out << reg_name(op.reg);
        break;
    case OperandKind::Imm:
        out << op.imm;
        break;
    case OperandKind::Mem:
        out << "dword [";
        {
            bool first = true;
            if (op.mem.hasBase) {
                out << reg_name(op.mem.base);
                first = false;
            }
            if (op.mem.hasSymbol) {
                if (!first) out << "+";
                out << op.mem.symbol;
                first = false;
            }
            if (op.mem.disp != 0 || first) {
                if (!first && op.mem.disp > 0) out << "+";
                out << op.mem.disp;
            }
        }
        out << "]";
        break;
    case OperandKind::Label:
    case OperandKind::Symbol:
        out << op.name;
        break;
    }
}

class AsmEmitter {
public:
    explicit AsmEmitter(std::ostream& o) : out(o) {}

    void emit_program(const IRProgram& p) {
        out << "format ELF\n\n";
        out << "section '.text' executable\n\n";
        out << "public main as 'main'\n\n";
    
        for (const auto& ins : p.instrs) {
            emit_instr(ins);
        }
        for (const auto& name : p.externs) {
            if (name == "ExitProcess@4") {
                out << "extrn 'ExitProcess' as ExitProcess\n";
            }
            else { out << "extrn '" << name << "' as " + name + "\n"; }
        }
        out << "\n";
    
        out << "section '.data' writeable\n";

        out << "    numfmt:   db \"%d\", 0x0\n";
        out << "    numfmtnl: db \"%d\", 0xa, 0x0\n";
        out << "    strfmt:   db \"%s\", 0x0\n";
    
        for (const auto& s : p.strings) {
            out << "    " << s.label << ": db ";
            GStringStream hex;
            for (char c : s.value) {
                hex << "0x" << std::hex << static_cast<int>(static_cast<unsigned char>(c)) << ", ";
            }
            out << hex.str() << "0x0\n";
        }

        out << "\nsection '.bss' writeable\n";
        out << "    tmp_stor: rd 1024\n";
        out << "    tmp_p:    rd 1\n";
        out << "    __BpmDoubleExceptionTypeId: rd 1\n";
        out << "    __BpmRecursionExceptionTypeId: rd 1\n";
        out << "    __BpmSigSegvExceptionTypeId: rd 1\n";
    
        for (const auto& g : p.globals) {
            out << "    v_" << g.name << ": rd 1\n";
        }
    }

private:
    std::ostream& out;

    void emit_instr(const IRInstr& ins) {
        switch (ins.op) {
        case IROp::Label:
            out << ins.a.name << ":\n";
            return;
        case IROp::Comment:
            out << "    ; " << ins.a.name << "\n";
            return;
        case IROp::Ret:
            out << "    ret\n";
            return;
    
        case IROp::Store8: {
            auto mem = ins.a;
            auto reg = ins.b;
            out << "    mov byte [";
            if (mem.kind == OperandKind::Mem) {
                bool first = true;
                if (mem.mem.hasBase) {
                    out << reg_name(mem.mem.base);
                    first = false;
                }
                if (mem.mem.hasSymbol) {
                    if (!first) out << "+";
                    out << mem.mem.symbol;
                    first = false;
                }
                if (mem.mem.disp != 0 || first) {
                    if (!first && mem.mem.disp > 0) out << "+";
                    out << mem.mem.disp;
                }
            }
            out << "], ";
    
            auto reg8_name = [](Reg r) -> const char* {
                switch (r) {
                case Reg::EAX: return "al";
                case Reg::EBX: return "bl";
                case Reg::ECX: return "cl";
                case Reg::EDX: return "dl";
                default:       return "al";
                }
            };
    
            if (reg.kind == OperandKind::Reg) {
                out << reg8_name(reg.reg);
            } else {
                print_operand(out, reg);
            }
            out << "\n";
            return;
        }
    
        case IROp::Store16: {
            auto mem = ins.a;
            auto reg = ins.b;
            out << "    mov word [";
            if (mem.kind == OperandKind::Mem) {
                bool first = true;
                if (mem.mem.hasBase) {
                    out << reg_name(mem.mem.base);
                    first = false;
                }
                if (mem.mem.hasSymbol) {
                    if (!first) out << "+";
                    out << mem.mem.symbol;
                    first = false;
                }
                if (mem.mem.disp != 0 || first) {
                    if (!first && mem.mem.disp > 0) out << "+";
                    out << mem.mem.disp;
                }
            }
            out << "], ";
    
            auto reg16_name = [](Reg r) -> const char* {
                switch (r) {
                case Reg::EAX: return "ax";
                case Reg::EBX: return "bx";
                case Reg::ECX: return "cx";
                case Reg::EDX: return "dx";
                default:       return "ax";
                }
            };
    
            if (reg.kind == OperandKind::Reg) {
                out << reg16_name(reg.reg);
            } else {
                print_operand(out, reg);
            }
            out << "\n";
            return;
        }
    
        case IROp::Load8: {
            auto dst = ins.a;
            auto mem = ins.b;
    
            auto reg8_name = [](Reg r) -> const char* {
                switch (r) {
                case Reg::EAX: return "al";
                case Reg::EBX: return "bl";
                case Reg::ECX: return "cl";
                case Reg::EDX: return "dl";
                default:       return "al";
                }
            };
    
            if (dst.kind == OperandKind::Reg && mem.kind == OperandKind::Mem) {
                out << "    xor " << reg_name(dst.reg) << ", " << reg_name(dst.reg) << "\n";
                out << "    mov " << reg8_name(dst.reg) << ", byte [";
                bool first = true;
                if (mem.mem.hasBase) {
                    out << reg_name(mem.mem.base);
                    first = false;
                }
                if (mem.mem.hasSymbol) {
                    if (!first) out << "+";
                    out << mem.mem.symbol;
                    first = false;
                }
                if (mem.mem.disp != 0 || first) {
                    if (!first && mem.mem.disp > 0) out << "+";
                    out << mem.mem.disp;
                }
                out << "]\n";
            }
            return;
        }
    
        case IROp::Load16: {
            auto dst = ins.a;
            auto mem = ins.b;
    
            auto reg16_name = [](Reg r) -> const char* {
                switch (r) {
                case Reg::EAX: return "ax";
                case Reg::EBX: return "bx";
                case Reg::ECX: return "cx";
                case Reg::EDX: return "dx";
                default:       return "ax";
                }
            };
    
            if (dst.kind == OperandKind::Reg && mem.kind == OperandKind::Mem) {
                out << "    xor " << reg_name(dst.reg) << ", " << reg_name(dst.reg) << "\n";
                out << "    mov " << reg16_name(dst.reg) << ", word [";
                bool first = true;
                if (mem.mem.hasBase) {
                    out << reg_name(mem.mem.base);
                    first = false;
                }
                if (mem.mem.hasSymbol) {
                    if (!first) out << "+";
                    out << mem.mem.symbol;
                    first = false;
                }
                if (mem.mem.disp != 0 || first) {
                    if (!first && mem.mem.disp > 0) out << "+";
                    out << mem.mem.disp;
                }
                out << "]\n";
            }
            return;
        }
    
        case IROp::InlineAsm: {
            out << "    " << ins.a.name << "\n";
            return;
        }
    
        default:
            out << "    " << opcode_name(ins.op) << " ";
            break;
        }
    
        bool hasA = ins.a.kind != OperandKind::None;
        bool hasB = ins.b.kind != OperandKind::None;
        bool hasC = ins.c.kind != OperandKind::None;
    
        if (hasA) {
            print_operand(out, ins.a);
            if (hasB) {
                out << ", ";
                print_operand(out, ins.b);
                if (hasC) {
                    out << ", ";
                    print_operand(out, ins.c);
                }
            }
        }
    
        out << "\n";
    }

    const char* opcode_name(IROp op) const {
        switch (op) {
        case IROp::Add:    return "add";
        case IROp::Sub:    return "sub";
        case IROp::Mul:    return "mul";
        case IROp::IMul:   return "imul";
        case IROp::Div:    return "div";
        case IROp::Xor:    return "xor";
        case IROp::Mov:    return "mov";
    
        case IROp::Store8:   return "";
        case IROp::Store16:  return "";
        case IROp::Load8:    return "";
        case IROp::Load16:   return "";
    
        case IROp::Shl:    return "shl";
        case IROp::Shr:    return "shr";
        case IROp::Cmp:    return "cmp";
        case IROp::CMovE:  return "cmove";
        case IROp::CMovNE: return "cmovne";
        case IROp::CMovL:  return "cmovl";
        case IROp::CMovG:  return "cmovg";
        case IROp::Test:   return "test";
        case IROp::Inc:    return "inc";
        case IROp::Jmp:    return "jmp";
        case IROp::Jz:     return "jz";
        case IROp::Jnz:    return "jnz";
        case IROp::Je:     return "je";
        case IROp::Jne:    return "jne";
        case IROp::Call:   return "call";
        case IROp::Push:   return "push";
        case IROp::Pop:    return "pop";
        case IROp::Ret:    return "ret";
    
        case IROp::Label:     return "";
        case IROp::Comment:   return "";
        case IROp::InlineAsm: return "";
        }
        return "";
    }   
};