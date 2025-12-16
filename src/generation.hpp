#pragma once


namespace INTERNAL_CODE {
    GString IMPLEMENTATION = R"(struct exception { __message: char*, __bstub1: int, __bstub2: int }
impl exception { proc what(exception self) -> char* { return self.__message; } }
struct __DoubleFreeException { __addr: ptr, __bstub1: int, __bstub2: int }
impl __DoubleFreeException { proc what(__DoubleFreeException self) -> char* {
        __pushonstack(self.__addr);
        asm "call __bpm_double_free_exception_what";
        asm "add esp, 4";
        asm "push eax";
        let __fst = __popfromstack();
        return cast(char*, __fst); } }
__oninit { __pushonstack(typeid(__DoubleFreeException)); asm "pop edx"; asm "mov dword [__BpmDoubleExceptionTypeId], edx"; }
struct __RecursionException { __bstub: int, __bstub1: int, __bstub2: int }
impl __RecursionException { proc what(__RecursionException self) -> char* {
        asm "call __bpm_recursion_exception_what";
        asm "push eax";
        let __fst = __popfromstack();
        return cast(char*, __fst); } }
__oninit { __pushonstack(typeid(__RecursionException)); asm "pop edx"; asm "mov dword [__BpmRecursionExceptionTypeId], edx"; }
struct __SigSegvException { __addr: int, __bstub1: int, __bstub2: int }
impl __SigSegvException { proc what(__SigSegvException self) -> char* {
        __pushonstack(self.__addr);
        asm "call __sigsegv_wh_exception";
        asm "add esp, 4";
        asm "push eax";
        let __fst = __popfromstack();
        return cast(char*, __fst); } }
__oninit { __pushonstack(typeid(__SigSegvException)); asm "pop edx"; asm "mov dword [__BpmSigSegvExceptionTypeId], edx"; }
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
    inline Operand label(const GString& s) const { return Operand::labelOp(s); }
    inline Operand sym(const GString& s) const   { return Operand::symbolOp(s); }
    inline Operand mem(const MemRef& m) const    { return Operand::memOp(m); }

    inline MemRef local_mem(size_t stack_loc) const {
        return MemRef::baseDisp(Reg::EBP, -static_cast<int32_t>(stack_loc));
    }
    inline MemRef global_mem(const GString& name) const {
        return MemRef::sym(name);
    }

    inline void push_reg(Reg r)                { m_builder.push(reg(r)); }
    inline void push_imm(int32_t v)            { m_builder.push(imm(v)); }
    inline void push_mem(const MemRef& m)      { m_builder.push(mem(m)); }
    inline void push_sym(const GString& s) { m_builder.push(sym(s)); }

    inline void pop_reg(Reg r)                 { m_builder.pop(reg(r)); }

    struct String {
        GString value;
        size_t index;
    };

    std::optional<String> string_lookup(const GString& svalue) noexcept {
        const auto& search = m_strings.find(svalue);
        if (search != m_strings.end()) return search->second;
        return std::nullopt;
    }

    inline void gen_push_str(const GString& value) {
        std::optional<String> str = string_lookup(value);
        size_t idx = 0;
        if (!str.has_value()) {
            idx = m_string_count++;
            m_strings[value] = { value, idx };
        } else {
            idx = str->index;
        }
        push_sym("s_" + GString(std::to_string(idx).c_str()));
    }

    explicit Generator(NodeProg* prog, DiagnosticManager* dman, ArenaAllocator* arenall, SemanticContext* sema) {
        m_prog = prog;
        m_diag_man = dman;
        m_allocator = arenall;
        m_sema = sema;
    }

    void DiagnosticMessage(const Token& tok, const GString& header, const GString& msg, const int col_inc) {
        m_diag_man->DiagnosticMessage(tok, header, msg, col_inc);
    }

    void GeneratorError(const Token& tok, const GString& msg) {
        m_diag_man->DiagnosticMessage(tok, "error", msg, 0);
        exit(EXIT_FAILURE);
    }

    void GeneratorWarning(const Token& tok, const GString& msg) {
        m_diag_man->DiagnosticMessage(tok, "warning", msg, 0);
    }

    void append_cextern(const GString& name) {
        if (std::find(m_cexterns.begin(), m_cexterns.end(), name) !=
            m_cexterns.end()) {
            return;
        }
        m_cexterns.push_back(name);
    }

    void gen_term(const NodeTerm* term, bool lvalue = false)
    {
        struct TermVisitor {
            Generator& gen;
            bool       lvalue;

            void operator()(const NodeTermIntLit* term_int_lit) const {
                int32_t v = std::stol(term_int_lit->int_lit.value.value().c_str());
                gen.push_imm(v);
            }

            void operator()([[maybe_unused]] const NodeTermType* tp) {

            }

            void operator()(const NodeTermCol* term_col) const {
                gen.push_imm(term_col->def.col);
            }

            void operator()(const NodeTermLine* term_line) const {
                gen.push_imm(term_line->def.line);
            }

            void operator()(const NodeTermPop* term_pop) const {
                static_cast<void>(term_pop);
            }

            void operator()(const NodeTermExprStmt* term_stmt) const {
                gen.gen_scope(term_stmt->scope);
                gen.gen_expr(term_stmt->expr);
            }

            void operator()(const NodeTermFile* term_file) const {
                gen.gen_push_str(term_file->def.file);
            }

            void operator()([[maybe_unused]] const NodeTermCtEval* term_eval) const {

            }

            void operator()(const NodeTermCtMdefined* term_mdef) const {
                gen.push_imm(static_cast<int32_t>(term_mdef->value));
            }

            void operator()([[maybe_unused]] const NodeTermSizeof* term_sizeof) const {

            }

            void operator()([[maybe_unused]] const NodeTermRd* term_rd) const {

            }

            void operator()(const NodeTermCast* term_cast) const {
                gen.gen_expr(term_cast->expr, lvalue);
            }

            void operator()(const NodeTermUnref* term_unref) const {
                gen.gen_expr(term_unref->expr);
                if(!lvalue) {
                    gen.pop_reg(Reg::ECX);
                    gen.push_mem(MemRef::baseDisp(Reg::ECX, 0));
                }
            }

            void operator()(const NodeTermCastTo* term_cast_to) const {
                gen.gen_expr(term_cast_to->expr, lvalue);
            }

            void operator()([[maybe_unused]] const NodeTermTypeid* term_typeid) const {
                
            }

            void operator()(const NodeTermStrLit* term_str_lit) const {
                gen.gen_push_str(term_str_lit->str_lit);
            }

            void operator()(const NodeTermAmpersand* term_amp) const {
                gen.gen_expr(term_amp->expr, true);
            }

            void operator()([[maybe_unused]] const NodeTermDrvalue* term_drval) const {

            }

            void operator()(NodeTermIdent* term_ident) const {
                const auto& search = gen.m_sema->m_sym_table.m_mapped_ident_symbols.find(term_ident);
                assert(search != gen.m_sema->m_sym_table.m_mapped_ident_symbols.end());
                TermIdentSymbol symbol = search->second;
                assert(symbol.symbol != NULL);
                if(symbol.kind == TermIdentSymbolKind::LOCAL_VAR) {
                    Variable* var = reinterpret_cast<Variable*>(symbol.symbol);
                    if (lvalue) {
                        gen.m_builder.mov(gen.reg(Reg::EDX), gen.reg(Reg::EBP));
                        gen.m_builder.sub(gen.reg(Reg::EDX), gen.imm(static_cast<int32_t>(var->stack_loc)));
                        gen.push_reg(Reg::EDX);
                    } else {
                        gen.push_mem(MemRef::baseDisp(Reg::EBP, -static_cast<int32_t>(var->stack_loc)));
                    }
                    return;
                }
                if(symbol.kind == TermIdentSymbolKind::GLOBAL_VAR) {
                    GlobalVariable* var = reinterpret_cast<GlobalVariable*>(symbol.symbol);
                    if (lvalue) {
                        gen.push_sym(var->mangled_symbol);
                    } else {
                        gen.push_mem(gen.global_mem(var->mangled_symbol));
                    }
                    return;
                }
            }

            void operator()(NodeTermNmIdent* term_ident) const {
                const auto& search = gen.m_sema->m_sym_table.m_mapped_nmident_symbols.find(term_ident);
                assert(search != gen.m_sema->m_sym_table.m_mapped_nmident_symbols.end());
                GlobalVariable* var = search->second;
                assert(var != nullptr);
                if(lvalue) gen.push_sym(var->mangled_symbol);
                else gen.push_mem(gen.global_mem(var->mangled_symbol));
            }

            void operator()(const NodeTermParen* term_paren) const {
                gen.gen_expr(term_paren->expr, lvalue);
            }

            void operator()(NodeTermNmCall* term_call) const {
                const auto& search = gen.m_sema->m_sym_table.m_mapped_nm_calls_symbols.find(term_call);
                assert(search != gen.m_sema->m_sym_table.m_mapped_nm_calls_symbols.end());
                Procedure* procedure = search->second;
                assert(procedure != nullptr);

                if(term_call->args.has_value()) gen.gen_expr(term_call->args.value());

                gen.m_builder.call(gen.sym(procedure->mangled_symbol));

                if(term_call->args.has_value()) gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(procedure->params.size() * 4));

                if(term_call->as_expr) gen.m_builder.push(gen.reg(Reg::EAX));
            }

            void operator()(NodeTermCall* term_call) const {
                const auto& search = gen.m_sema->m_sym_table.m_mapped_calls_symbols.find(term_call);
                assert(search != gen.m_sema->m_sym_table.m_mapped_calls_symbols.end());
                if(std::holds_alternative<Procedure*>(search->second)) {
                    Procedure* procedure = std::get<Procedure*>(search->second);
                    assert(procedure != nullptr);
                    if(term_call->args.has_value()) gen.gen_expr(term_call->args.value());
                    gen.m_builder.call(gen.sym(procedure->mangled_symbol));
                    if(term_call->args.has_value()) gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(procedure->params.size() * 4));
                    if(term_call->as_expr) gen.m_builder.push(gen.reg(Reg::EAX));
                    return;
                }
                if(std::holds_alternative<Struct*>(search->second)) {
                    Struct* structure = std::get<Struct*>(search->second);
                    assert(structure != nullptr);
                    size_t size_of_structure = structure->fields.size() * 4ULL;

                    bool has_arguments = term_call->args.has_value();
                    size_t args_len = 0ULL;
                    if(has_arguments) {
                        GVector<NodeExpr*> args_exprs = gen.m_sema->__getargs(term_call->args.value());
                        gen.gen_expr(term_call->args.value());
                        args_len = args_exprs.size();
                    }

                    gen.push_imm(size_of_structure);
                    gen.m_builder.call(gen.sym("memalloc"));
                    gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(4));
                    if(has_arguments) {
                        for(size_t i = 0;i < args_len; ++i) {
                            gen.pop_reg(Reg::EDX);
                            gen.m_builder.mov(gen.mem(MemRef::baseDisp(Reg::EAX, i * 4)), gen.reg(Reg::EDX));
                        }
                    }
                    gen.push_reg(Reg::EAX);
                    return;
                }
                assert(false);
            }

            void operator()([[maybe_unused]] const NodeTermMtCall* term_call) const {
               
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
                gen.gen_expr(sub->lhs);
                gen.gen_expr(sub->rhs);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.sub(gen.reg(Reg::EAX), gen.reg(Reg::EBX));
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprAdd* add) const {
                gen.gen_expr(add->lhs);
                gen.gen_expr(add->rhs);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.add(gen.reg(Reg::EAX), gen.reg(Reg::EBX));
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprMulti* multi) const {
                gen.gen_expr(multi->lhs);
                gen.gen_expr(multi->rhs);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.emit(IRInstr(IROp::Mul, gen.reg(Reg::EBX)));
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprDiv* div) const {
                gen.gen_expr(div->lhs);
                gen.gen_expr(div->rhs);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.emit(IRInstr(IROp::Xor, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.emit(IRInstr(IROp::Div, gen.reg(Reg::EBX)));
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprShl* shl) const {
                gen.gen_expr(shl->lhs);
                gen.gen_expr(shl->rhs);
                gen.pop_reg(Reg::ECX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.emit(IRInstr(IROp::Shl, gen.reg(Reg::EAX), gen.reg(Reg::CL)));
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprShr* shr) const {
                gen.gen_expr(shr->lhs);
                gen.gen_expr(shr->rhs);
                gen.pop_reg(Reg::ECX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.emit(IRInstr(IROp::Shr, gen.reg(Reg::EAX), gen.reg(Reg::CL)));
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprMod* md) const {
                gen.gen_expr(md->lhs);
                gen.gen_expr(md->rhs);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.emit(IRInstr(IROp::Xor, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.emit(IRInstr(IROp::Div, gen.reg(Reg::EBX)));
                gen.push_reg(Reg::EDX);
            }

            void operator()(const NodeBinExprEqEq* eqeq) const {
                gen.gen_expr(eqeq->lhs);
                gen.gen_expr(eqeq->rhs);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.mov(gen.reg(Reg::EDX), gen.imm(0));
                gen.m_builder.mov(gen.reg(Reg::ECX), gen.imm(1));
                gen.m_builder.emit(IRInstr(IROp::Cmp, gen.reg(Reg::EAX), gen.reg(Reg::EBX)));
                gen.m_builder.emit(IRInstr(IROp::CMovE, gen.reg(Reg::EDX), gen.reg(Reg::ECX)));
                gen.push_reg(Reg::EDX);
            }

            void operator()(const NodeBinExprNotEq* nq) const {
                gen.gen_expr(nq->lhs);
                gen.gen_expr(nq->rhs);
                gen.pop_reg(Reg::EAX);
                gen.pop_reg(Reg::EBX);
                gen.m_builder.mov(gen.reg(Reg::EDX), gen.imm(0));
                gen.m_builder.mov(gen.reg(Reg::ECX), gen.imm(1));
                gen.m_builder.emit(IRInstr(IROp::Cmp, gen.reg(Reg::EAX), gen.reg(Reg::EBX)));
                gen.m_builder.emit(IRInstr(IROp::CMovNE, gen.reg(Reg::EDX), gen.reg(Reg::ECX)));
                gen.push_reg(Reg::EDX);
            }

            void operator()(const NodeBinExprLess* less) const {
                gen.gen_expr(less->lhs);
                gen.gen_expr(less->rhs);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.mov(gen.reg(Reg::EDX), gen.imm(0));
                gen.m_builder.mov(gen.reg(Reg::ECX), gen.imm(1));
                gen.m_builder.emit(IRInstr(IROp::Cmp, gen.reg(Reg::EAX), gen.reg(Reg::EBX)));
                gen.m_builder.emit(IRInstr(IROp::CMovL, gen.reg(Reg::EDX), gen.reg(Reg::ECX)));
                gen.push_reg(Reg::EDX);
            }

            void operator()(const NodeBinExprAnd* band) const {
                gen.gen_expr(band->lhs);
                GString flab = gen.create_label();
                GString elab = gen.create_label();
                gen.pop_reg(Reg::EDX);
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.jz(gen.label(flab));
                gen.gen_expr(band->rhs);
                gen.pop_reg(Reg::EDX);
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.jz(gen.label(flab));
                gen.m_builder.mov(gen.reg(Reg::EAX), gen.imm(1));
                gen.m_builder.jmp(gen.label(elab));
                gen.m_builder.label(flab);
                gen.m_builder.mov(gen.reg(Reg::EAX), gen.imm(0));
                gen.m_builder.label(elab);
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprOr* bor) const {
                GString flab = gen.create_label();
                GString elab = gen.create_label();
                GString tlab = gen.create_label();
                gen.gen_expr(bor->lhs);
                gen.pop_reg(Reg::EDX);
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.jnz(gen.label(tlab));
                gen.gen_expr(bor->rhs);
                gen.pop_reg(Reg::EDX);
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.jz(gen.label(flab));
                gen.m_builder.label(tlab);
                gen.m_builder.mov(gen.reg(Reg::EAX), gen.imm(1));
                gen.m_builder.jmp(gen.label(elab));
                gen.m_builder.label(flab);
                gen.m_builder.mov(gen.reg(Reg::EAX), gen.imm(0));
                gen.m_builder.label(elab);
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprAbove* above) const {
                gen.gen_expr(above->lhs);
                gen.gen_expr(above->rhs);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.mov(gen.reg(Reg::EDX), gen.imm(0));
                gen.m_builder.mov(gen.reg(Reg::ECX), gen.imm(1));
                gen.m_builder.emit(IRInstr(IROp::Cmp, gen.reg(Reg::EAX), gen.reg(Reg::EBX)));
                gen.m_builder.emit(IRInstr(IROp::CMovG, gen.reg(Reg::EDX), gen.reg(Reg::ECX)));
                gen.push_reg(Reg::EDX);
            }

            void operator()(const NodeBinExprDot* dot) const {
                assert(dot->resolved_field != nullptr); // semantic analyze must resolve this field access

                gen.gen_expr(dot->lhs);
                gen.pop_reg(Reg::EBX);
                size_t field_offset = dot->resolved_field->nth * 4;
                if(lvalue) {
                    if(field_offset > 0) gen.m_builder.add(gen.reg(Reg::EBX), gen.imm(field_offset));
                    gen.push_reg(Reg::EBX);
                }
                else {
                    gen.push_mem(MemRef::baseDisp(Reg::EBX, field_offset));
                }
            }

            void operator()(const NodeBinExprArgs* args) const {
                for (int i = static_cast<int>(args->args.size()) - 1; i > -1; --i) {
                    gen.gen_expr(args->args[i]);
                }
            }
            void operator()([[maybe_unused]] const NodeBinExprIndex* idx) const {
                
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

    void gen_scope(const NodeScope* scope)
    {
        for (const NodeStmt* stmt : scope->stmts) {
            gen_stmt(stmt);
        }
    }

    void gen_if_pred(const NodeIfPred* pred, const GString& end_label)
    {
        struct PredVisitor {
            Generator& gen;
            const GString&  end_label;

            void operator()([[maybe_unused]] const NodeIfPredElif* elif) const
            {
                gen.gen_expr(elif->expr);
                gen.pop_reg(Reg::EAX);
                const GString label = gen.create_label();
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EAX), gen.reg(Reg::EAX)));
                gen.m_builder.jz(gen.label(label));
                gen.gen_scope(elif->scope);
                gen.m_builder.jmp(gen.label(end_label));
                gen.m_builder.label(label);
                if (elif->pred.has_value()) {
                    gen.gen_if_pred(elif->pred.value(), end_label);
                }
            }

            void operator()([[maybe_unused]] const NodeIfPredElse* else_) const
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
            }

            void operator()(NodeStmtProc* stmt_proc)
            {
                if(stmt_proc->templates != NULL) return;
                if(stmt_proc->prototype) {
                    bool cimport = std::find(stmt_proc->attrs.begin(), stmt_proc->attrs.end(), ProcAttr::cimport) != stmt_proc->attrs.end();
                    if(cimport) {
                        gen.append_cextern(stmt_proc->name);
                    }
                    return;
                }
                const auto& search = gen.m_sema->m_sym_table.m_mapped_procs_symbols.find(stmt_proc);
                assert(search != gen.m_sema->m_sym_table.m_mapped_procs_symbols.end());
                Procedure* symbol = search->second;
                assert(symbol != nullptr);
                gen.m_builder.label(symbol->mangled_symbol);
                bool noprolog = std::find(stmt_proc->attrs.begin(), stmt_proc->attrs.end(), ProcAttr::noprolog) != stmt_proc->attrs.end();
                if(!noprolog) {
                    gen.m_builder.push(gen.reg(Reg::EBP));
                    gen.m_builder.mov(gen.reg(Reg::EBP), gen.reg(Reg::ESP));
                    if(symbol->max_stack_allign > 0) gen.m_builder.sub(gen.reg(Reg::ESP), gen.imm(symbol->max_stack_allign));
                    gen.gen_push_str(symbol->name);
                    gen.m_builder.call(gen.sym("__bpm_proc_enter"));
                    gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(4));
                }
                symbol->return_label = gen.create_label();

                if(std::find(stmt_proc->attrs.begin(), stmt_proc->attrs.end(), ProcAttr::nostdargs) == stmt_proc->attrs.end()) {
                    for (int i = 0; i < static_cast<int>(symbol->params.size()); i++) {
                        gen.m_builder.mov(
                            gen.reg(Reg::EDX),
                            gen.mem(MemRef::baseDisp(Reg::EBP, i * 4 + 8))
                        );
                        gen.m_builder.mov(
                            gen.mem(MemRef::baseDisp(Reg::EBP, -(i * 4 + 4))),
                            gen.reg(Reg::EDX)
                        );
                    }
                }

                gen.gen_scope(stmt_proc->scope);

                gen.m_builder.label(symbol->return_label);
                if(!noprolog) {
                    gen.m_builder.call(gen.sym("__bpm_proc_leave"));
                    if(symbol->rettype != BaseDataTypeVoid) gen.m_builder.pop(gen.reg(Reg::EAX));
                    gen.m_builder.leave();
                }
                gen.m_builder.ret();
            }

            void operator()(NodeStmtReturn* stmt_return) const
            {
                if(stmt_return->expr.has_value()) {
                    gen.gen_expr(stmt_return->expr.value());
                }
                const auto& search = gen.m_sema->m_sym_table.m_mapped_return_symbols.find(stmt_return);
                assert(search != gen.m_sema->m_sym_table.m_mapped_return_symbols.end());
                Procedure* symbol = search->second;
                assert(symbol != nullptr);
                gen.m_builder.jmp(gen.label(symbol->return_label));
            }

            void operator()(NodeStmtLet* stmt_let) const
            {
                if(!stmt_let->expr.has_value()) return;
                const auto& search = gen.m_sema->m_sym_table.m_mapped_let_symbols.find(stmt_let);
                assert(search != gen.m_sema->m_sym_table.m_mapped_let_symbols.end());
                TermIdentSymbol symbol = search->second;
                
                if(symbol.kind == TermIdentSymbolKind::GLOBAL_VAR) {
                    GlobalVariable* var = reinterpret_cast<GlobalVariable*>(symbol.symbol);
                    gen.m_init_globals.push_back({var, stmt_let->expr.value()});
                    return;
                }

                assert(symbol.symbol != NULL);
                gen.gen_expr(stmt_let->expr.value());
                gen.m_builder.pop(gen.reg(Reg::EBX));
                MemRef mem {};
                if(symbol.kind == TermIdentSymbolKind::LOCAL_VAR) {
                    Variable* var = reinterpret_cast<Variable*>(symbol.symbol);
                    mem = MemRef::baseDisp(Reg::EBP, -static_cast<int32_t>(var->stack_loc));
                }
                gen.m_builder.mov(gen.mem(mem), gen.reg(Reg::EBX));
            }

            void operator()([[maybe_unused]] const NodeStmtCompileTimeIf* stmt_ctif) const
            {
                 
            }

            void operator()(const NodeStmtAssign* stmt_assign) const
            {
                gen.gen_expr(stmt_assign->lvalue, true);
                gen.gen_expr(stmt_assign->expr);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.mov(
                    gen.mem(MemRef::baseDisp(Reg::EAX, 0)),
                    gen.reg(Reg::EBX)
                );
            }

            void operator()(const NodeStmtIncBy* stmt_assign) const
            {
                gen.gen_expr(stmt_assign->lvalue, true);
                gen.gen_expr(stmt_assign->expr);
                gen.pop_reg(Reg::ECX);
                gen.pop_reg(Reg::EDX);
                gen.m_builder.add(
                    gen.mem(MemRef::baseDisp(Reg::EDX, 0)),
                    gen.reg(Reg::ECX)
                );
            }

            void operator()([[maybe_unused]] const NodeStmtDecBy* stmt_assign) const
            {

            }

            void operator()([[maybe_unused]] const NodeStmtMulBy* stmt_assign) const
            {
                
            }

            void operator()([[maybe_unused]] const NodeStmtDivBy* stmt_assign) const
            {
               
            }

            void operator()(NodeStmtCall* stmt_call) const
            {
                assert(stmt_call->resolved_expression != nullptr);
                gen.gen_expr(stmt_call->resolved_expression);
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
                gen.gen_expr(stmt_if->expr);
                gen.pop_reg(Reg::EAX);
                const GString label = gen.create_label();
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EAX), gen.reg(Reg::EAX)));
                gen.m_builder.jz(gen.label(label));
                gen.gen_scope(stmt_if->scope);
                if (stmt_if->pred.has_value()) {
                    const GString end_label = gen.create_label();
                    gen.m_builder.jmp(gen.label(end_label));
                    gen.m_builder.label(label);
                    gen.gen_if_pred(stmt_if->pred.value(), end_label);
                    gen.m_builder.label(end_label);
                } else {
                    gen.m_builder.label(label);
                }
            }

            void operator()(NodeStmtWhile* stmt_while) const
            {
                auto preiflab = gen.create_label();
                auto blocklab = gen.create_label();
                auto endlab   = gen.create_label();
                stmt_while->break_label = endlab;

                gen.m_builder.label(preiflab);
                gen.gen_expr(stmt_while->expr);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EAX), gen.reg(Reg::EAX)));
                gen.m_builder.jz(gen.label(endlab));

                gen.m_builder.label(blocklab);
                gen.gen_scope(stmt_while->scope);
                gen.m_builder.jmp(gen.label(preiflab));

                gen.m_builder.label(endlab);
            }

            void operator()(const NodeStmtBreak* stmt_break) const
            {
                assert(stmt_break->from != nullptr);
                gen.m_builder.jmp(gen.label(stmt_break->from->break_label));
            }

            void operator()([[maybe_unused]] const NodeStmtStore* stmt_store) const
            {
                
            }

            void operator()([[maybe_unused]] const NodeStmtBuffer* stmt_buf) const
            {
               
            }

            void operator()(const NodeStmtAsm* stmt_asm) const
            {
                gen.m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp(stmt_asm->code)));
            }

            void operator()(const NodeStmtCextern* stmt_cextern) const
            {
                gen.append_cextern(stmt_cextern->name);
            }

            void operator()([[maybe_unused]] const NodeStmtStruct* stmt_struct) const
            {
               
            }

            void operator()([[maybe_unused]] const NodeStmtInterface* stmt_inter) const
            {
                
            }

            void operator()([[maybe_unused]] const NodeStmtOninit* stmt_oninit) const
            {
               
            }

            void operator()([[maybe_unused]] const NodeStmtStaticAssert* stmt_st) const
            {
               
            }

            void operator()([[maybe_unused]] const NodeStmtDelete* stmt_delete) const
            {
                
            }

            void operator()([[maybe_unused]] const NodeStmtRaise* stmt_raise) const
            {
               
            }

            void operator()(const NodeStmtNamespace* stmt_space) const
            {
                gen.gen_scope(stmt_space->scope);
            }

            void operator()(const NodeStmtImpl* stmt_impl) const
            {
                gen.gen_scope(stmt_impl->scope);
            }

            void operator()(const NodeStmtNmCall* stmt_call) const
            {
                assert(stmt_call->resolved_expression != nullptr);
                gen.gen_expr(stmt_call->resolved_expression);
            }

            void operator()([[maybe_unused]] const NodeStmtMtCall* stmt_call) const
            {
               
            }

            void operator()([[maybe_unused]] const NodeStmtConst* stmt_const) const
            {
               
            }

            void operator()([[maybe_unused]] const NodeStmtTypedef* stmt_tdef) const
            {
              
            }

            void operator()([[maybe_unused]] const NodeStmtTry* stmt_try) const
            {
                
            }
            void operator()([[maybe_unused]] const NodeStmtFor* stmt_for) const
            {
                
            }
            void operator()([[maybe_unused]] const NodeStmtForeach* stmt_foreach) const
            {
            
            }
            void operator()([[maybe_unused]] const NodeStmtEnum* stmt_enum) const
            {
   
            }
        };

        StmtVisitor visitor{ *this };
        std::visit(visitor, stmt->var);
    }

    [[nodiscard]] GString gen_prog()
	{
	    IRProgram main_ir;
	
	    m_main_ir  = &main_ir;
	    m_builder  = IRBuilder(m_main_ir);
	
	    m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("public __BpmDoubleExceptionTypeId")));
	    m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("public __BpmRecursionExceptionTypeId")));
	    m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("public __BpmSigSegvExceptionTypeId")));
	
	    for (const NodeStmt* stmt : m_prog->stmts) {
	        gen_stmt(stmt);
	    }

        m_builder.label("_BPM_init_");
        m_builder.push( Operand::regOp(Reg::EBP) );
        m_builder.mov(
            Operand::regOp(Reg::EBP),
            Operand::regOp(Reg::ESP)
        );

        for(auto&& p : m_init_globals) {
            gen_expr(p.second);
            pop_reg(Reg::EDX);
            m_builder.mov(mem(global_mem(p.first->mangled_symbol)), reg(Reg::EDX));
        }
        m_builder.leave();
        m_builder.ret();

	    m_builder.label("main");
        m_builder.push( Operand::regOp(Reg::EBP) );

        m_builder.mov(
            Operand::regOp(Reg::EBP),
            Operand::regOp(Reg::ESP)
        );

        m_builder.push( Operand::memOp( MemRef::baseDisp(Reg::EBP, 12) ) );
        m_builder.push( Operand::memOp( MemRef::baseDisp(Reg::EBP, 8) ) );

        m_builder.call(sym("_BPM_init_"));

	    m_builder.call(sym("__bpm_set_sigsegv_handler"));
	    m_builder.push(reg(Reg::EBP));
        m_builder.call(sym("gc_set_stack_base"));
        m_builder.add(reg(Reg::ESP), imm(4));

	    m_builder.mov(mem(MemRef::sym("__BpmDoubleExceptionTypeId")),   imm(0));
	    m_builder.mov(mem(MemRef::sym("__BpmRecursionExceptionTypeId")), imm(0));
	    m_builder.mov(mem(MemRef::sym("__BpmSigSegvExceptionTypeId")),  imm(0));

	    m_builder.call(sym(m_sema->m_entry_point->mangled_symbol));
        m_builder.add(reg(Reg::ESP), imm(8));

	    m_builder.push(reg(Reg::EAX));
	    m_builder.call(sym("ExitProcess"));
	
    	for (auto& ext : m_cexterns)
    	    main_ir.externs.push_back(ext);
	
    	for (auto&& p : m_strings) {
    	    main_ir.strings.push_back({
    	        "s_" + GString(std::to_string(p.second.index).c_str()),
    	        p.second.value
    	    });
    	}
	
    	for (auto&& p : m_sema->m_sym_table.m_gvars) {
    	    main_ir.globals.push_back({ p.second->mangled_symbol });
    	}
	    
        if (m_optimize) {
            iropt::optimize_ir(main_ir);
        }
    	GStringStream result_ss;
    	AsmEmitter emitter(result_ss);
    	emitter.emit_program(main_ir);
    	return result_ss.str().c_str();
	}

    bool m_optimize = false;

private:

    inline GString create_label() noexcept
    {
        return ".L" + GString(std::to_string(m_label_count++).c_str());
    }

    GVector<GString> m_cexterns = {
        "ExitProcess",
        "__bpm_set_sigsegv_handler",
        "gc_set_stack_base",
        "__bpm_proc_enter",
        "__bpm_proc_leave",
        "memalloc",
        "memfree",
    };

    GVector<std::pair<GlobalVariable*, NodeExpr*>> m_init_globals {};

    GMap<GString, String> m_strings {};

    size_t m_label_count = 0;
    size_t m_string_count = 0;

    const NodeProg* m_prog = nullptr;

    SemanticContext* m_sema = nullptr;

	IRProgram* m_main_ir  = nullptr;
	IRBuilder m_builder {nullptr};

    DiagnosticManager* m_diag_man = nullptr;

    ArenaAllocator* m_allocator = nullptr;
};