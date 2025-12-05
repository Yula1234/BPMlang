#pragma once


#define __DTOR_PREFIX "__dtor__"

#define v_alt(__v, __tp) std::holds_alternative<__tp>(__v)
#define v_get(__v, __tp) std::get<__tp>(__v)

#define UNUSED_ARG __attribute__((unused))

using __str_ref = const std::string&;

void consume_un(...) {}

namespace INTERNAL_CODE {
    std::string IMPLEMENTATION = R"(struct exception { __message: char*, __bstub1: int, __bstub2: int }
impl exception { proc what(exception self) -> char* { return self.__message; } }
struct __DoubleFreeException { __addr: ptr, __bstub1: int, __bstub2: int }
impl __DoubleFreeException { proc what(__DoubleFreeException self) -> char* {
        __pushonstack(self.__addr);
        asm "call __bpm_double_free_exception_what";
        asm "add esp, 4";
        asm "push eax";
        let __fst = __popfromstack();
        return cast(char*, __fst); } }
__oninit { __pushonstack(typeid(__DoubleFreeException)); asm "pop edx"; asm "mov dword [___BpmDoubleExceptionTypeId], edx"; }
struct __RecursionException { __bstub: int, __bstub1: int, __bstub2: int }
impl __RecursionException { proc what(__RecursionException self) -> char* {
        asm "call __bpm_recursion_exception_what";
        asm "push eax";
        let __fst = __popfromstack();
        return cast(char*, __fst); } }
__oninit { __pushonstack(typeid(__RecursionException)); asm "pop edx"; asm "mov dword [___BpmRecursionExceptionTypeId], edx"; }
struct __SigSegvException { __addr: int, __bstub1: int, __bstub2: int }
impl __SigSegvException { proc what(__SigSegvException self) -> char* {
        __pushonstack(self.__addr);
        asm "call __sigsegv_wh_exception";
        asm "add esp, 4";
        asm "push eax";
        let __fst = __popfromstack();
        return cast(char*, __fst); } }
__oninit { __pushonstack(typeid(__SigSegvException)); asm "pop edx"; asm "mov dword [___BpmSigSegvExceptionTypeId], edx"; }
namespace std { proc exception(char* mess_) -> exception = return exception(mess_, 0, 0); }
interface __ObjectTypeI {}
interface __SimpleTypeI {}
interface __PointerTypeI {}
interface __StdAddable<T> { proc m_add(self, T other) -> T; }
interface __StdSubstractable<T> { proc m_sub(self, T other) -> T; }
interface __StdMultipliable<T> { proc m_mul(self, T other) -> T; }
interface __StdDivisible<T> { proc m_div(self, T other) -> T; }
interface __StdEquatable<T> { proc m_equal(self, T other) -> int; }
interface __StdAssignable<T> { proc m_assign(self, T other) -> void; }
proc memcpy(ptr __src, ptr __dst, int __size) -> void;)";
}

class Generator {
public:
    inline Operand reg(Reg r) const              { return Operand::regOp(r); }
    inline Operand imm(int32_t v) const          { return Operand::immOp(v); }
    inline Operand label(const std::string& s) const { return Operand::labelOp(s); }
    inline Operand sym(const std::string& s) const   { return Operand::symbolOp(s); }
    inline Operand mem(const MemRef& m) const    { return Operand::memOp(m); }

    inline MemRef local_mem(size_t stack_loc) const {
        return MemRef::baseDisp(Reg::EBP, -static_cast<int32_t>(stack_loc));
    }
    inline MemRef global_mem(const std::string& name) const {
        return MemRef::sym("v_" + name);
    }

    inline void push_reg(Reg r)                { m_builder.push(reg(r)); }
    inline void push_imm(int32_t v)            { m_builder.push(imm(v)); }
    inline void push_mem(const MemRef& m)      { m_builder.push(mem(m)); }
    inline void push_sym(const std::string& s) { m_builder.push(sym(s)); }

    inline void pop_reg(Reg r)                 { m_builder.pop(reg(r)); }

    explicit Generator(NodeProg* prog, DiagnosticManager* dman, ArenaAllocator* arenall) {
        m_prog = prog;
        m_diag_man = dman;
        m_allocator = arenall;
    }

    Generator(const Generator& other) {
	    m_typeid_table   = other.m_typeid_table;
        m_allocator      = other.m_allocator;
	    m_diag_man       = other.m_diag_man;
	    m_label_count    = other.m_label_count;
	
	    m_main_ir  = other.m_main_ir;
	    m_templ_ir = other.m_templ_ir;
	    m_builder  = IRBuilder(m_main_ir); 
	}

    void DiagnosticMessage(const Token& tok, __str_ref header, __str_ref msg, const int col_inc) {
        m_diag_man->DiagnosticMessage(tok, header, msg, col_inc);
    }

    void GeneratorError(const Token& tok, __str_ref msg) {
        m_diag_man->DiagnosticMessage(tok, "error", msg, 0);

        if (m_cur_proc.has_value()) {
            const Procedure& p = m_cur_proc.value();
            std::string detail = "in function `" + p.name + "`";
            m_diag_man->DiagnosticMessage(p.def, "note", detail, 0, false);
        }

        exit(EXIT_FAILURE);
    }

    void GeneratorWarning(const Token& tok, __str_ref msg) {
        m_diag_man->DiagnosticMessage(tok, "warning", msg, 0);
    }


    void gen_traceback_push(Procedure& proc) {
        gen_push_str(proc.name);
        m_builder.call(sym("traceback_push"));
        m_builder.add(reg(Reg::ESP), imm(4));
    }

    void gen_traceback_push_nm(Procedure& proc, __str_ref nm) {
        gen_push_str(nm + "::" + proc.name);
        m_builder.call(sym("traceback_push"));
        m_builder.add(reg(Reg::ESP), imm(4));
    }

    void gen_term(const NodeTerm* term, bool lvalue = false)
    {
        struct TermVisitor {
            Generator& gen;
            bool       lvalue;

            void operator()(const NodeTermIntLit* term_int_lit) const {
                if (lvalue)
                    gen.GeneratorError(term_int_lit->int_lit, "can't use integer constant as lvalue expression (any operations that taking addres).");
                int32_t v = std::stol(term_int_lit->int_lit.value.value());
                gen.push_imm(v);
            }

            void operator()(const NodeTermType* tp) {
                gen.GeneratorError(tp->def, "`type` only can be used in context of compile-time expressions");
            }

            void operator()(const NodeTermCol* term_col) const {
                gen.push_imm(term_col->def.col);
            }

            void operator()(const NodeTermLine* term_line) const {
                gen.push_imm(term_line->def.line);
            }

            void operator()(const NodeTermPop* term_pop) const {
                consume_un(term_pop);
            }

            void operator()(const NodeTermExprStmt* term_stmt) const {
                gen.gen_scope(term_stmt->scope);
                gen.gen_expr(term_stmt->expr);
            }

            void operator()(const NodeTermFile* term_file) const {
                gen.gen_push_str(term_file->def.file);
            }

            void operator()(const NodeTermCtEval* term_eval) const {
                gen.GeneratorError(term_eval->def, "ct_eval is deprecated");
            }

            void operator()(const NodeTermCtMdefined* term_mdef) const {
                gen.push_imm(static_cast<int32_t>(term_mdef->value));
            }

            void operator()(NodeTermSizeof* term_sizeof) const {
                if (term_sizeof->expr.has_value()) {
                    DataType tp = gen.type_of_expr(term_sizeof->expr.value());
                    size_t   sz = gen.sizeof_of(tp);
                    if (!sz) {
                        gen.GeneratorError(term_sizeof->def, "please provide a type to sizeof, or close expression to parens.");
                    }
                    gen.push_imm(static_cast<int32_t>(sz));
                } else {
                    DataType dt = term_sizeof->type;
                    gen.substitute_template(dt);
                    size_t sz = gen.sizeof_of(dt);
                    if (!sz) {
                        gen.GeneratorError(term_sizeof->def, "please provide a type to sizeof, or close expression to parens.");
                    }
                    gen.push_imm(static_cast<int32_t>(sz));
                }
            }

            void operator()(const NodeTermRd* term_rd) const {
                gen.gen_expr(term_rd->expr);
                switch (term_rd->size) {
                case 8: {
                    gen.pop_reg(Reg::EDX);
                    Operand dst = gen.reg(Reg::ECX);
                    Operand m   = gen.mem(MemRef::baseDisp(Reg::EDX, 0));
                    gen.m_builder.emit(IRInstr(IROp::Load8, dst, m));
                    gen.push_reg(Reg::ECX);
                    break;
                }
                case 16: {
                    gen.pop_reg(Reg::EDX);
                    Operand dst = gen.reg(Reg::ECX);
                    Operand m   = gen.mem(MemRef::baseDisp(Reg::EDX, 0));
                    gen.m_builder.emit(IRInstr(IROp::Load16, dst, m));
                    gen.push_reg(Reg::ECX);
                    break;
                }
                case 32: {
                    gen.pop_reg(Reg::EDX);
                    Operand m = gen.mem(MemRef::baseDisp(Reg::EDX, 0));
                    gen.m_builder.mov(gen.reg(Reg::ECX), m);
                    gen.push_reg(Reg::ECX);
                    break;
                }
                default:
                    assert(false);
                }
            }

            void operator()(const NodeTermCast* term_cast) const {
                gen.gen_expr(term_cast->expr, lvalue);
            }

            void operator()(const NodeTermUnref* term_unref) const {
                DataType tp = gen.type_of_expr(term_unref->expr);
                if (!tp.root().link && tp.root().ptrlvl == 0 && !tp.is_object() && tp.root().getsimpletype() != SimpleDataType::ptr) {
                    gen.GeneratorError(term_unref->def, "can't dereference not-reference or pointer type `" + tp.to_string() + "`");
                }
                if (tp.is_object() && !tp.root().link && tp.root().ptrlvl == 0) {
                    if (lvalue) {
                        NodeTermMtCall deref;
                        deref.def = term_unref->def;
                        deref.mt = term_unref->expr;
                        deref.name = "m_assign_deref";
                        std::vector<NodeExpr*> args;
                        args.push_back(term_unref->expr);
                        NodeBinExprArgs bargs{ .args = args };
                        NodeBinExpr bexpr{ .def = term_unref->def, .var = &bargs };
                        NodeExpr eargs{ .var = &bexpr };
                        deref.args = &eargs;
                        NodeTerm asterm{ .var = &deref };
                        NodeExpr asexpr{ .var = &asterm };
                        gen.gen_expr(&asexpr);
                    } else {
                        NodeTermMtCall deref;
                        deref.def = term_unref->def;
                        deref.mt = term_unref->expr;
                        deref.name = "m_deref";
                        std::vector<NodeExpr*> args;
                        args.push_back(term_unref->expr);
                        NodeBinExprArgs bargs{ .args = args };
                        NodeBinExpr bexpr{ .def = term_unref->def, .var = &bargs };
                        NodeExpr eargs{ .var = &bexpr };
                        deref.args = &eargs;
                        NodeTerm asterm{ .var = &deref };
                        NodeExpr asexpr{ .var = &asterm };
                        gen.gen_expr(&asexpr);
                    }
                    return;
                }
                gen.gen_expr(term_unref->expr, lvalue);
                gen.pop_reg(Reg::EDX);
                Operand m = gen.mem(MemRef::baseDisp(Reg::EDX, 0));
                gen.m_builder.mov(gen.reg(Reg::ECX), m);
                gen.push_reg(Reg::ECX);
            }

            void operator()(const NodeTermCastTo* term_cast_to) const {
                gen.gen_expr(term_cast_to->expr, lvalue);
            }

            void operator()(NodeTermTypeid* term_typeid) const {
                if (term_typeid->ptype.has_value()) {
                    gen.push_imm(static_cast<int32_t>(gen.typeid_of(term_typeid->ptype.value())));
                } else {
                    gen.push_imm(static_cast<int32_t>(gen.typeid_of(gen.type_of_expr(term_typeid->expr))));
                }
            }

            void operator()(const NodeTermStrLit* term_str_lit) const {
                gen.gen_push_str(term_str_lit->str_lit);
            }

            void operator()(const NodeTermAmpersand* term_amp) const {
                gen.gen_expr(term_amp->expr, true);
            }

            void operator()(const NodeTermDrvalue* term_drval) const {
                if (lvalue)
                    gen.GeneratorError(term_drval->def, "using __disable_rvalue__ as rvalue");
                gen.gen_expr(term_drval->expr, false);
            }

            void operator()(const NodeTermIdent* term_ident) const {
                std::string name = term_ident->ident.value.value();
                if (auto it = gen.var_lookup(name)) {
                    const Var& v = it.value();
                    if (lvalue) {
                        gen.m_builder.mov(gen.reg(Reg::EDX), gen.reg(Reg::EBP));
                        gen.m_builder.sub(gen.reg(Reg::EDX), gen.imm(static_cast<int32_t>(v.stack_loc)));
                        gen.push_reg(Reg::EDX);
                    } else {
                        gen.push_mem(v.mem());
                    }
                    return;
                }
                if (auto cns = gen.const_lookup(name)) {
                    if (lvalue) {
                        gen.GeneratorError(term_ident->ident, "can't get addres or reference of compile-time constant.");
                    }
                    gen.push_imm(cns->value);
                    return;
                }
                if (auto prc = gen.proc_lookup(name)) {
                    gen.push_sym(prc->name);
                    return;
                }
                if (auto ivar = gen.gvar_lookup(name)) {
                    if (lvalue) {
                        gen.push_sym("v_" + ivar->name);
                    } else {
                        gen.push_mem(gen.global_mem(ivar->name));
                    }
                    return;
                }
                gen.GeneratorError(term_ident->ident, "unkown word `" + name + "`");
            }

            void operator()(const NodeTermParen* term_paren) const {
                gen.gen_expr(term_paren->expr, lvalue);
            }

            void operator()(NodeTermNmCall* term_call) const {
                __str_ref pname = term_call->name;
                __str_ref nname = term_call->nm;
                std::optional<Namespace*> nms = gen.namespace_lookup(nname);
                if (!nms.has_value())
                    gen.GeneratorError(term_call->def, "unkown namespace `" + nname + "`");
                const auto& search = nms.value()->procs.find(pname);
                if (search == nms.value()->procs.end())
                    gen.GeneratorError(term_call->def, "namespace `" + nname + "` doesn't have procedure `" + pname + "`");
                Procedure proc = search->second;
                if (!proc.overrides.empty()) {
                    gen.resolve_overrides_tp(&proc, term_call->args, term_call->def, term_call->targs);
                }
                std::vector<NodeExpr*> raw_args;
                if (term_call->args.has_value()) raw_args = gen.__getargs(term_call->args.value());
                
                auto inst_res = gen.instantiate_if_needed(proc, term_call->targs, raw_args, term_call->def, pname, nname);
                std::string tsign = inst_res.first;
                auto temps = inst_res.second;

                if (proc.rettype.root() == BaseDataTypeVoid)
                    gen.GeneratorError(term_call->def, "can't use void " + nname + "::" + pname + "(...) as value");
                
                size_t stack_allign = 0;
                Procedure proc_check = proc;
                gen.substitute_template_params(temps, proc_check.params);

                if (!raw_args.empty() || proc.params.empty()) {
                    if (!raw_args.empty()) gen.__typecheck_call(raw_args, proc_check.params, term_call->def, proc_check, &stack_allign);
                } else {
                    gen.GeneratorError(term_call->def, "procedure `" + proc.name + "` expects " + std::to_string(proc.params.size()) + " args, but got 0");
                }
                
                gen.gen_args(raw_args, proc_check.params, term_call->def);
                
                std::string label = gen.mangle_ns_name(nname) + "@" + pname + tsign;
                if (proc.override) label += proc.get_sign();
                gen.m_builder.call(gen.sym(label));
                if (stack_allign != 0) {
                    gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(static_cast<int32_t>(stack_allign * 4)));
                }
                gen.push_reg(Reg::EAX);
            }

            void operator()(NodeTermCall* term_call) const {
                const std::string name = term_call->name;
                std::optional<Var> var = gen.var_lookup(name);
                if (var.has_value() && 
                    var.value().type.root().is_simple() &&
                    var.value().type.root().getsimpletype() == SimpleDataType::proc_ptr) {
                    
                    DataType funcType = var.value().type;
                    const auto& gens = funcType.node->generics;
                    
                    if (gens.empty()) {
                        gen.GeneratorError(term_call->def, "ProcPtr without return type signature");
                    }
                    
                    DataType retType = gens[0];
                    std::vector<DataType> paramTypes;
                    for(size_t i = 1; i < gens.size(); ++i) paramTypes.push_back(gens[i]);
                    
                    std::vector<NodeExpr*> callArgs;
                    if (term_call->args.has_value()) callArgs = gen.__getargs(term_call->args.value());
                    
                    if (callArgs.size() != paramTypes.size()) {
                         gen.GeneratorError(term_call->def, "indirect call expects " + std::to_string(paramTypes.size()) + " args, got " + std::to_string(callArgs.size()));
                    }
                    
                    for (int i = static_cast<int>(callArgs.size()) - 1; i >= 0; --i) {
                        gen.gen_expr(callArgs[i]); 
                    }
                    
                    gen.m_builder.mov(gen.reg(Reg::EAX), gen.mem(var.value().mem()));
                    
                    gen.m_builder.call(gen.reg(Reg::EAX));
                    
                    if (!callArgs.empty()) {
                        gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(callArgs.size() * 4));
                    }
                    
                    gen.push_reg(Reg::EAX);
                    return;
                }
                
                std::optional<Procedure> _proc = gen.proc_lookup(name);
                if (_proc.has_value()) {
                    Procedure proc = _proc.value();
                    if (proc.rettype == BaseDataTypeConst) {
                        int res{ 0 };
                        Executor executor(gen, term_call->def);
                        try { executor.execute(proc); }
                        catch (Executor::ReturnException& ret) { res = ret.get_value(); }
                        gen.push_imm(res);
                        return;
                    }
                    if (!proc.overrides.empty()) {
                        gen.resolve_overrides_tp(&proc, term_call->args, term_call->def, term_call->targs);
                    }
                    if (proc.rettype.root() == BaseDataTypeVoid)
                        gen.GeneratorError(term_call->def, "can't use void " + term_call->name + "(...) as value");
                    std::vector<NodeExpr*> raw_args;
                    if (term_call->args.has_value()) raw_args = gen.__getargs(term_call->args.value());
                    
                    auto inst_res = gen.instantiate_if_needed(proc, term_call->targs, raw_args, term_call->def, name, "");
                    std::string tsign = inst_res.first;
                    auto temps = inst_res.second;
                    
                    size_t stack_allign = 0;
                    Procedure proc_check = proc;
                    gen.substitute_template_params(temps, proc_check.params);

                    if (!raw_args.empty() || proc.params.empty()) {
                        if (!raw_args.empty()) gen.__typecheck_call(raw_args, proc_check.params, term_call->def, proc_check, &stack_allign);
                    } else {
                        gen.GeneratorError(term_call->def, "procedure `" + proc.name + "` expects " + std::to_string(proc.params.size()) + " args, but got 0");
                    }
                    
                    gen.gen_args(raw_args, proc_check.params, term_call->def);
                    
                    std::string label = name + tsign;
                    if (proc.override) label += proc.get_sign();
                    gen.m_builder.call(gen.sym(label));
                    if (stack_allign != 0) {
                        gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(static_cast<int32_t>(stack_allign * 4)));
                    }
                    gen.push_reg(Reg::EAX);
                    return;
                }
                std::optional<Struct> st = gen.struct_lookup(term_call->name);
                if (st.has_value()) {
                    if (st.value().temp && term_call->targs.size() != st.value().temps.size())
                        gen.GeneratorError(term_call->def, "struct `" + st.value().name + "` except " + std::to_string(st.value().temps.size()) + " template arguments in <...>, bug got " + std::to_string(term_call->targs.size()) + ".");
                    size_t objectSize = st.value().fields.size();
                    if (objectSize == 0U) {
                        gen.GeneratorError(term_call->def, "try to allocate zero-sized type.");
                    }
                    gen.push_imm(static_cast<int32_t>(objectSize * 4U));
                    if (st.value().has_allocator()) {
                        gen.m_builder.call(gen.sym(st.value().__allocator.value()));
                    } else {
                        gen.m_builder.call(gen.sym("memalloc"));
                    }
                    gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(4));
                    bool eax_break = false;
                    std::vector<NodeExpr*> iargs;
                    if (term_call->args.has_value()) {
                        if (std::holds_alternative<NodeBinExpr*>(term_call->args.value()->var)) {
                            NodeBinExpr* binargs = std::get<NodeBinExpr*>(term_call->args.value()->var);
                            if (std::holds_alternative<NodeBinExprArgs*>(binargs->var)) {
                                NodeBinExprArgs* args = std::get<NodeBinExprArgs*>(binargs->var);
                                iargs = args->args;
                            } else {
                                iargs.push_back(term_call->args.value());
                            }
                        } else {
                            if (iargs.size() == 0U) {
                                iargs.push_back(term_call->args.value());
                            }
                        }
                    }
                    if (!iargs.empty()) {
                        if (iargs.size() != st.value().fields.size()) {
                            gen.GeneratorError(term_call->def, "except " + std::to_string(st.value().fields.size()) + " args\nNOTE: but got " + std::to_string(iargs.size()) + "\nNOTE: if you don't want initialize all fields dont provide any arguments");
                        }
                        eax_break = true;
                        gen.m_builder.mov(gen.reg(Reg::EDX), gen.mem(MemRef::sym("tmp_p")));
                        gen.m_builder.add(gen.reg(Reg::EDX), gen.imm(4));
                        gen.m_builder.mov(gen.mem(MemRef::sym("tmp_p")), gen.reg(Reg::EDX));
                        gen.m_builder.mov(
                            gen.mem(MemRef::baseSym(Reg::EDX, "tmp_stor")),
                            gen.reg(Reg::EAX)
                        );

                        Struct _st = st.value();
                        __map<std::string, DataType> temps;
                        if (_st.temp) {
                            size_t counter{ 0 };
                            for (auto&& el : _st.temps) {
                                DataType targ = term_call->targs[counter++];
                                gen.substitute_template(targ);
                                targ = gen.canonical_type(targ);
                                temps[el] = targ;
                            }
                        }
                        for (int i = 0; i < static_cast<int>(iargs.size()); ++i) {
                            DataType itype = gen.type_of_expr(iargs[i]);
                            DataType ftype = _st.__fields[i].second;
                            if (_st.temp)
                                gen.substitute_template_wct(ftype, temps);
                            if (itype != ftype) {
                                gen.GeneratorError(term_call->def, "missmatch in initializers types for field nth `" + std::to_string(i + 1) + "`\nNOTE: field name - `" + _st.__fields[i].first + "`" + "\nNOTE: excepted " + ftype.to_string() + "\nNOTE: but got " + itype.to_string());
                            }
                            gen.gen_expr(iargs[i]);
                            gen.pop_reg(Reg::ECX);
                            gen.m_builder.mov(gen.reg(Reg::EBX), gen.mem(MemRef::sym("tmp_p")));
                            gen.m_builder.mov(gen.reg(Reg::EDX), gen.mem(MemRef::baseSym(Reg::EBX, "tmp_stor")));
                            gen.m_builder.mov(
                                gen.mem(MemRef::baseDisp(Reg::EDX, i * 4)),
                                gen.reg(Reg::ECX)
                            );
                        }
                    }
                    if (!eax_break) {
                        gen.push_reg(Reg::EAX);
                    } else {
                        gen.m_builder.mov(gen.reg(Reg::EDX), gen.mem(MemRef::sym("tmp_p")));
                        gen.push_mem(MemRef::baseSym(Reg::EDX, "tmp_stor"));
                        gen.m_builder.sub(gen.reg(Reg::EDX), gen.imm(4));
                        gen.m_builder.mov(gen.mem(MemRef::sym("tmp_p")), gen.reg(Reg::EDX));
                    }
                    return;
                }
                gen.GeneratorError(term_call->def, "unkown procedure `" + name + "`");
            }

            void operator()(const NodeTermMtCall* term_call) const {
                DataType tpof = gen.type_of_expr(term_call->mt);
                if (!tpof.root().is_object || tpof.root().link)
                    gen.GeneratorError(term_call->def, "can't call method from type " + tpof.to_string() + ".");
                NodeTermNmCall nmcall;
                nmcall.def = term_call->def;
                nmcall.nm = tpof.root().getobjectname();
                nmcall.name = term_call->name;
                nmcall.args = term_call->args;
                nmcall.targs = term_call->targs;
                NodeTerm term;
                term.var = &nmcall;
                gen.gen_term(&term);
            }
            void operator()(const NodeTermNmIdent* nm_ident) const {
                std::optional<Namespace*> nms = gen.namespace_lookup(nm_ident->nm);
                if (!nms.has_value()) {
                    gen.GeneratorError(nm_ident->def, "unkown namespace `" + nm_ident->nm + "`");
                }
                
                Namespace* ns = nms.value();
                auto it = ns->consts.find(nm_ident->name);
                
                if (it != ns->consts.end()) {
                    gen.push_imm(it->second);
                    return;
                }

                // TODO: global variables in namespaces
                
                gen.GeneratorError(nm_ident->def, "unkown member `" + nm_ident->name + "` in namespace `" + nm_ident->nm + "`");
            }
        };

        TermVisitor visitor{ *this, lvalue };
        std::visit(visitor, term->var);
    }

    void gen_bin_expr(const NodeBinExpr* bin_expr, bool lvalue = false)
    {
        struct BinExprVisitor {
            Generator& gen;
            bool       lvalue;
            const NodeBinExpr* base;

            void operator()(const NodeBinExprSub* sub) const {

            }

            void operator()(const NodeBinExprAdd* add) const {

            }

            void operator()(const NodeBinExprMulti* multi) const {

            }

            void operator()(const NodeBinExprDiv* div) const {

            }

            void operator()(const NodeBinExprShl* shl) const {

            }

            void operator()(const NodeBinExprShr* shr) const {

            }

            void operator()(const NodeBinExprMod* md) const {

            }

            void operator()(const NodeBinExprEqEq* eqeq) const {

            }

            void operator()(const NodeBinExprNotEq* nq) const {

            }

            void operator()(const NodeBinExprLess* less) const {

            }

            void operator()(const NodeBinExprAnd* band) const {

            }

            void operator()(const NodeBinExprOr* bor) const {

            }

            void operator()(const NodeBinExprAbove* above) const {

            }

            void operator()(const NodeBinExprDot* dot) const {

            }

            void operator()(const NodeBinExprArgs* args) const {

            }
            void operator()(const NodeBinExprIndex* idx) const {

            }
        };

        BinExprVisitor visitor{ *this, lvalue, bin_expr };
        std::visit(visitor, bin_expr->var);
    }

    void gen_expr(const NodeExpr* expr, bool lvalue = false)
    {
        struct ExprVisitor {
            Generator& gen;
            bool       lvalue;

            void operator()(const NodeTerm* term) const
            {
                gen.gen_term(term, lvalue);
            }

            void operator()(const NodeBinExpr* bin_expr) const
            {
                gen.gen_bin_expr(bin_expr, lvalue);
            }
        };

        ExprVisitor visitor{ *this, lvalue };
        std::visit(visitor, expr->var);
    }

    void gen_if_pred(const NodeIfPred* pred, __str_ref end_label)
    {
        struct PredVisitor {
            Generator& gen;
            __str_ref  end_label;

            void operator()(const NodeIfPredElif* elif) const
            {

            }

            void operator()(const NodeIfPredElse* else_) const
            {
                gen.gen_scope(else_->scope);
            }
        };

        PredVisitor visitor{ *this, end_label };
        std::visit(visitor, pred->var);
    }

    void gen_stmt(const NodeStmt* stmt)
    {
        struct StmtVisitor {
            Generator& gen;

            void operator()(const NodeStmtExit* stmt_exit) const
            {
                gen.gen_expr(stmt_exit->expr);
                gen.m_builder.call(gen.sym("ExitProcess"));
            }

            void operator()(const NodeStmtProc* stmt_proc)
            {

            }

            void operator()(const NodeStmtReturn* stmt_return) const
            {
                
            }

            void operator()(const NodeStmtLet* stmt_let) const
            {
                
            }

            void operator()(const NodeStmtLetNoAssign* stmt_let) const
            {

            }

            void operator()(const NodeStmtCompileTimeIf* stmt_ctif) const {

            }

            void operator()(const NodeStmtAssign* stmt_assign) const
            {

            }

            void operator()(const NodeStmtIncBy* stmt_assign) const
            {

            }

            void operator()(const NodeStmtDecBy* stmt_assign) const
            {

            }

            void operator()(const NodeStmtMulBy* stmt_assign) const
            {

            }

            void operator()(const NodeStmtDivBy* stmt_assign) const
            {

            }

            void operator()(NodeStmtCall* stmt_call) const
            {

            }

            void operator()(const NodeScope* scope) const
            {
                gen.gen_scope(scope);
            }

            void operator()(const NodeStmtPushOnStack* stmt_push) const
            {
                gen.gen_expr(stmt_push->expr);
            }

            void operator()(const NodeStmtIf* stmt_if) const
            {

            }

            void operator()(const NodeStmtWhile* stmt_while) const
            {

            }

            void operator()(const NodeStmtBreak* stmt_break) const
            {

            }

            void operator()(const NodeStmtStore* stmt_store) const
            {

            }

            void operator()(const NodeStmtBuffer* stmt_buf) const
            {

            }

            void operator()(const NodeStmtAsm* stmt_asm) const
            {
                gen.m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp(stmt_asm->code)));
            }

            void operator()(const NodeStmtCextern* stmt_cextern) const
            {

            }

            void operator()(const NodeStmtStruct* stmt_struct) const
            {

            }

            void operator()(const NodeStmtInterface* stmt_inter) const
            {

            }

            void operator()(const NodeStmtOninit* stmt_oninit) const
            {
                gen.__oninits.push_back(stmt_oninit->scope);
            }

            void operator()(const NodeStmtStaticAssert* stmt_st) const
            {

            }

            void operator()(const NodeStmtDelete* stmt_delete) const
            {

            }

            void operator()(const NodeStmtRaise* stmt_raise) const
            {

            }

            void operator()(const NodeStmtNamespace* stmt_space) const
            {

            }

            void operator()(NodeStmtImpl* stmt_impl) const
            {

            }

            void operator()(NodeStmtNmCall* stmt_call) const
            {

            }

            void operator()(const NodeStmtMtCall* stmt_call) const
            {

            }

            void operator()(const NodeStmtConst* stmt_const) const
            {

            }

            void operator()(const NodeStmtTypedef* stmt_tdef) const
            {

            }

            void operator()(const NodeStmtTry* stmt_try) const
            {

            }
            void operator()(const NodeStmtFor* stmt_for) const
            {

            }
            void operator()(const NodeStmtForeach* stmt_foreach) const
            {

            }
            void operator()(const NodeStmtEnum* stmt_enum) const
            {

            }
        };

        StmtVisitor visitor{ *this };
        std::visit(visitor, stmt->var);
    }

    [[nodiscard]] std::string gen_prog()
	{
	    m_consts.push_back({});
	    m_typedefs.push_back({});
	
	    IRProgram main_ir;
	    IRProgram templ_ir;
	
	    m_main_ir  = &main_ir;
	    m_templ_ir = &templ_ir;
	    m_builder  = IRBuilder(m_main_ir);

	    std::vector<PendingTemplateInstance> pending;
    	m_pending_templates = &pending;
	
	    m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("public ___BpmDoubleExceptionTypeId")));
	    m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("public ___BpmRecursionExceptionTypeId")));
	    m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("public ___BpmSigSegvExceptionTypeId")));
	
	    for (const NodeStmt* stmt : m_prog->stmts) {
	        gen_stmt(stmt);
	    }

	    gen_all_template_instances();

	    m_builder.label("main");
        m_builder.push( Operand::regOp(Reg::EBP) );

        m_builder.mov(
            Operand::regOp(Reg::EBP),
            Operand::regOp(Reg::ESP)
        );

        m_builder.push( Operand::memOp( MemRef::baseDisp(Reg::EBP, 12) ) );
        m_builder.push( Operand::memOp( MemRef::baseDisp(Reg::EBP, 8) ) );

	    m_builder.call(sym("__bpm_set_sigsegv_handler"));
	    m_builder.push(reg(Reg::EBP));
        m_builder.call(sym("gc_set_stack_base"));
        m_builder.add(reg(Reg::ESP), imm(4));

	    m_builder.mov(mem(MemRef::sym("___BpmDoubleExceptionTypeId")),   imm(0));
	    m_builder.mov(mem(MemRef::sym("___BpmRecursionExceptionTypeId")), imm(0));
	    m_builder.mov(mem(MemRef::sym("___BpmSigSegvExceptionTypeId")),  imm(0));

	    m_builder.push(imm(static_cast<int32_t>((*m_typeid_table_size) * 4ULL)));
	    m_builder.call(sym("malloc"));
	    m_builder.add(reg(Reg::ESP), imm(4));
	    m_builder.mov(mem(MemRef::sym("__type_id_table")), reg(Reg::EAX));

	    //m_init_typeid_table_ir();

	    m_builder.call(sym("__main"));
        m_builder.add(reg(Reg::ESP), imm(8));

	    m_builder.push(imm(0));
	    m_builder.call(sym("ExitProcess"));

	    bool has_oninits = !__oninits.empty();

	    m_builder.label("_BPM_init_");
	    if (has_oninits) {
	        m_builder.push(reg(Reg::EBP));
	        m_builder.mov(reg(Reg::EBP), reg(Reg::ESP));
	    }

	    m_builder.mov(mem(MemRef::sym("tmp_p")), imm(0));

	    for (const NodeScope* scope : __oninits) {
	        gen_scope(scope);
	    }

	    if (has_oninits) {
	        m_builder.pop(reg(Reg::EBP));
	    }
	    m_builder.ret();

	    IRProgram final_ir;

    	final_ir.instrs.insert(final_ir.instrs.end(),
    	                       m_main_ir->instrs.begin(), m_main_ir->instrs.end());
    	final_ir.instrs.insert(final_ir.instrs.end(),
    	                       m_templ_ir->instrs.begin(), m_templ_ir->instrs.end());
	
	    if (m_optimize) {
            iropt::optimize_ir(final_ir);
        }
    	std::stringstream result_ss;
    	AsmEmitter emitter(result_ss);
    	emitter.emit_program(final_ir);
    	return result_ss.str();
	}

    bool m_optimize = false;

private:

    std::vector<std::string> m_cexterns {
        "ExitProcess@4",
    };

    const NodeProg* m_prog = nullptr;

    std::vector<NodeScope*> __oninits;

	IRProgram* m_main_ir  = nullptr;
	IRProgram* m_templ_ir = nullptr;
	
	IRBuilder m_builder{ nullptr };

    DiagnosticManager* m_diag_man = nullptr;

    ArenaAllocator* m_allocator = nullptr;
};