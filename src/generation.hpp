#pragma once

#include <algorithm>
#include <cassert>

#include "parser.hpp"

class Generator {
public:
    struct Var {
        std::string name {};
        size_t stack_loc {};
        DataType type {};
    };
    struct Procedure {
        std::string name {};
        std::vector<std::pair<std::string, DataType>> params {};
        DataType rettype {};
        size_t stack_allign;
    };
    struct String {
        std::string value {};
        size_t index {};
    };
    explicit Generator(NodeProg prog)
        : m_prog(std::move(prog))
    {
    }

    std::optional<Var> var_lookup(std::string name) {
        Var it;
        bool finded = false;
        for(Var var : m_vars) {
            if(var.name == name) {
                it = var;
                finded = true;
            }
        }
        if(finded) {
            return it;
        }
        return std::nullopt;
    }

    std::optional<Procedure> proc_lookup(std::string name) {
        Procedure it;
        bool finded = false;
        for(Procedure var : m_procs) {
            if(var.name == name) {
                it = var;
                finded = true;
            }
        }
        if(finded) {
            return it;
        }
        return std::nullopt;
    }

    std::optional<String> string_lookup(std::string svalue) {
        String it;
        bool finded = false;
        for(String var : m_strings) {
            if(var.value == svalue) {
                it = var;
                finded = true;
            }
        }
        if(finded) {
            return it;
        }
        return std::nullopt;
    }

    void GeneratorError(Token tok, std::string msg) {
        putloc(tok);
        std::cout << " ERROR: " << msg << "\n";
        exit(EXIT_FAILURE);
    }

    DataType type_of_expr(const NodeExpr* expr) {
        if(holds_alternative<NodeTerm*>(expr->var)) {
            NodeTerm* term = std::get<NodeTerm*>(expr->var);
            if(std::holds_alternative<NodeTermIntLit*>(term->var)) {
                return DataType::_int;
            }
            if(std::holds_alternative<NodeTermStrLit*>(term->var)) {
                return DataType::ptr;
            }
            if(std::holds_alternative<NodeTermRd*>(term->var)) {
                return DataType::_int;
            }
            if(std::holds_alternative<NodeTermParen*>(term->var)) {
                return type_of_expr(std::get<NodeTermParen*>(term->var)->expr);
            }
            if(std::holds_alternative<NodeTermAmpersand*>(term->var)) {
                return DataType::ptr;
            }
            if(std::holds_alternative<NodeTermCall*>(term->var)) {
                NodeTermCall* call = std::get<NodeTermCall*>(term->var);
                std::string name = call->name;
                std::optional<Procedure> proc = proc_lookup(name);
                if(!proc.has_value()) {
                    return DataType::_void;
                }
                return proc.value().rettype;
            }
            if(std::holds_alternative<NodeTermIdent*>(term->var)) {
                std::optional<Var> svar = var_lookup(std::get<NodeTermIdent*>(term->var)->ident.value.value());
                if(!svar.has_value()) {
                    return DataType::_int;
                }
                return svar.value().type;
            }
        }
        static_assert(BinaryOpsCount == 7,
                    "\n     Impl type_of for new binop");
        if(holds_alternative<NodeBinExpr*>(expr->var)) {
            NodeBinExpr* binex = std::get<NodeBinExpr*>(expr->var);
            if(std::holds_alternative<NodeBinExprAdd*>(binex->var)) {
                return type_of_expr(std::get<NodeBinExprAdd*>(binex->var)->lhs);
            }
            if(std::holds_alternative<NodeBinExprMulti*>(binex->var)) {
                return type_of_expr(std::get<NodeBinExprMulti*>(binex->var)->lhs);
            }
            if(std::holds_alternative<NodeBinExprSub*>(binex->var)) {
                return type_of_expr(std::get<NodeBinExprSub*>(binex->var)->lhs);
            }
            if(std::holds_alternative<NodeBinExprDiv*>(binex->var)) {
                return type_of_expr(std::get<NodeBinExprDiv*>(binex->var)->lhs);
            }
            if(std::holds_alternative<NodeBinExprEqEq*>(binex->var)) {
                return type_of_expr(std::get<NodeBinExprEqEq*>(binex->var)->lhs);
            }
            if(std::holds_alternative<NodeBinExprNotEq*>(binex->var)) {
                return type_of_expr(std::get<NodeBinExprNotEq*>(binex->var)->lhs);
            }
            if(std::holds_alternative<NodeBinExprLess*>(binex->var)) {
                return type_of_expr(std::get<NodeBinExprLess*>(binex->var)->lhs);
            }
            if(std::holds_alternative<NodeBinExprAbove*>(binex->var)) {
                return type_of_expr(std::get<NodeBinExprAbove*>(binex->var)->lhs);
            }
        }
        assert(false);
    }

    bool typecheck_bin_expr(const NodeBinExpr* expr) {
        static_assert(BinaryOpsCount == 7,
                    "\n     Impl typecheck for new binop");
        if(std::holds_alternative<NodeBinExprAdd*>(expr->var)) {
            NodeBinExprAdd* add = std::get<NodeBinExprAdd*>(expr->var);
            return type_of_expr(add->lhs) == type_of_expr(add->rhs);
        } else if(std::holds_alternative<NodeBinExprSub*>(expr->var)) {
            NodeBinExprSub* sub = std::get<NodeBinExprSub*>(expr->var);
            return type_of_expr(sub->lhs) == type_of_expr(sub->rhs);
        } else if(std::holds_alternative<NodeBinExprMulti*>(expr->var)) {
            NodeBinExprMulti* mul = std::get<NodeBinExprMulti*>(expr->var);
            return type_of_expr(mul->lhs) == type_of_expr(mul->rhs);
        } else if(std::holds_alternative<NodeBinExprDiv*>(expr->var)) {
            NodeBinExprDiv* div = std::get<NodeBinExprDiv*>(expr->var);
            return type_of_expr(div->lhs) == type_of_expr(div->rhs);
        } else if(std::holds_alternative<NodeBinExprEqEq*>(expr->var)) {
            NodeBinExprEqEq* eqeq = std::get<NodeBinExprEqEq*>(expr->var);
            return type_of_expr(eqeq->lhs) == type_of_expr(eqeq->rhs);
        } else if(std::holds_alternative<NodeBinExprNotEq*>(expr->var)) {
            NodeBinExprNotEq* nq = std::get<NodeBinExprNotEq*>(expr->var);
            return type_of_expr(nq->lhs) == type_of_expr(nq->rhs);
        } else if(std::holds_alternative<NodeBinExprLess*>(expr->var)) {
            NodeBinExprLess* less = std::get<NodeBinExprLess*>(expr->var);
            return type_of_expr(less->lhs) == type_of_expr(less->rhs);
        } else if(std::holds_alternative<NodeBinExprAbove*>(expr->var)) {
            NodeBinExprAbove* above = std::get<NodeBinExprAbove*>(expr->var);
            return type_of_expr(above->lhs) == type_of_expr(above->rhs);
        } else {
            assert(false);
        }
    }

    void typecheck_bin_expr_err(const NodeBinExpr* expr, std::string IRexpr) {
        static_assert(BinaryOpsCount == 7,
                    "\n     Impl typecheck_err for new binop");
        if(!typecheck_bin_expr(expr)) {
            DataType ltype;
            DataType rtype;
            if(std::holds_alternative<NodeBinExprAdd*>(expr->var)) {
                const NodeBinExprAdd* add = std::get<NodeBinExprAdd*>(expr->var);
                ltype = type_of_expr(add->lhs);
                rtype = type_of_expr(add->rhs);
            } else if(std::holds_alternative<NodeBinExprSub*>(expr->var)) {
                const NodeBinExprSub* sub = std::get<NodeBinExprSub*>(expr->var);
                ltype = type_of_expr(sub->lhs);
                rtype = type_of_expr(sub->rhs);
            } else if(std::holds_alternative<NodeBinExprMulti*>(expr->var)) {
                const NodeBinExprMulti* mul = std::get<NodeBinExprMulti*>(expr->var);
                ltype = type_of_expr(mul->lhs);
                rtype = type_of_expr(mul->rhs);
            } else if(std::holds_alternative<NodeBinExprDiv*>(expr->var)) {
                const NodeBinExprDiv* div = std::get<NodeBinExprDiv*>(expr->var);
                ltype = type_of_expr(div->lhs);
                rtype = type_of_expr(div->rhs);
            } else if(std::holds_alternative<NodeBinExprEqEq*>(expr->var)) {
                const NodeBinExprEqEq* eqeq = std::get<NodeBinExprEqEq*>(expr->var);
                ltype = type_of_expr(eqeq->lhs);
                rtype = type_of_expr(eqeq->rhs);
            } else if(std::holds_alternative<NodeBinExprLess*>(expr->var)) {
                const NodeBinExprLess* less = std::get<NodeBinExprLess*>(expr->var);
                ltype = type_of_expr(less->lhs);
                rtype = type_of_expr(less->rhs);
            } else if(std::holds_alternative<NodeBinExprAbove*>(expr->var)) {
                const NodeBinExprAbove* above = std::get<NodeBinExprAbove*>(expr->var);
                ltype = type_of_expr(above->lhs);
                rtype = type_of_expr(above->rhs);
            } else if(std::holds_alternative<NodeBinExprNotEq*>(expr->var)) {
                const NodeBinExprNotEq* nq = std::get<NodeBinExprNotEq*>(expr->var);
                ltype = type_of_expr(nq->lhs);
                rtype = type_of_expr(nq->rhs);
            } else {
                assert(false);
            }
            if(!(ltype == DataType::ptr && rtype == DataType::_int)) {
                GeneratorError(expr->def, "can't use `" + IRexpr + "` for types " + dt_to_string(ltype) + " and " + dt_to_string(rtype));
            }
        }
    }

    void gen_term(const NodeTerm* term, bool lvalue = false)
    {
        struct TermVisitor {
            Generator& gen;
            bool lvalue;

            void operator()(const NodeTermIntLit* term_int_lit) const
            {
                gen.push(term_int_lit->int_lit.value.value());
            }

            void operator()(const NodeTermRd* term_rd) const
            {
                if(term_rd->size == 8) {
                    gen.gen_expr(term_rd->expr);
                    gen.m_output << "    pop edx\n";
                    gen.m_output << "    xor ecx, ecx\n";
                    gen.m_output << "    mov cl, byte [edx]\n";
                    gen.m_output << "    push ecx\n";
                } else if(term_rd->size == 16) {
                    gen.gen_expr(term_rd->expr);
                    gen.m_output << "    pop edx\n";
                    gen.m_output << "    xor ecx, ecx\n";
                    gen.m_output << "    mov cx, word [edx]\n";
                    gen.m_output << "    push ecx\n";
                } else if(term_rd->size == 32) {
                    gen.gen_expr(term_rd->expr);
                    gen.m_output << "    pop edx\n";
                    gen.m_output << "    push dword [edx]\n";
                } else {
                    assert(false); // unreacheable
                }
            }

            void operator()(const NodeTermStrLit* term_str_lit) const
            {
                std::string value = term_str_lit->str_lit.value.value();
                std::optional<String> str = gen.string_lookup(value);
                if(!str.has_value()) {
                    size_t index = gen.m_strings.size();
                    gen.m_strings.push_back({ .value = value, .index = index});
                    gen.m_output << "    push str_" << index << "\n";
                    return;
                }
                gen.m_output << "    push str_" << str.value().index << "\n";
            }

            void operator()(const NodeTermAmpersand* term_amp) const
            {
                gen.gen_expr(term_amp->expr, true);
            }

            void operator()(const NodeTermIdent* term_ident) const
            {
                Var it;
                bool finded = false;
                for(Var var : gen.m_vars) {
                    if(var.name == term_ident->ident.value.value()) {
                        it = var;
                        finded = true;
                    }
                }
                if (!finded) {
                    gen.GeneratorError(term_ident->ident, "unkown word `" + term_ident->ident.value.value() + "`");
                }
                if(lvalue) {
                    gen.m_output << "    mov edx, ebp\n";
                    gen.m_output << "    sub edx, " << it.stack_loc << "\n";
                    gen.m_output << "    push edx\n";
                } else {
                    std::stringstream offset;
                    offset << "dword [ebp-" << it.stack_loc << "]";
                    gen.push(offset.str());
                }
            }

            void operator()(const NodeTermParen* term_paren) const
            {
                gen.gen_expr(term_paren->expr);
            }

            void operator()(const NodeTermCall* term_call) const
            {
                const std::string name = term_call->def.value.value();
                std::optional<Procedure> proc = gen.proc_lookup(name);
                if(!proc.has_value()) {
                    gen.GeneratorError(term_call->def, "unkown procedure `" + name + "`");
                }
                size_t stack_allign = 0;
                if(term_call->args.has_value()) {
                    if(proc.value().params.size() == 0) {
                        gen.GeneratorError(term_call->def, "procedure `" + name + "` don't excepts any arguments");
                    }
                    NodeExpr* targs = term_call->args.value();
                    NodeExpr* args = std::get<NodeTermParen*>(std::get<NodeTerm*>(targs->var)->var)->expr;
                    if(std::holds_alternative<NodeBinExpr*>(args->var)) {
                        NodeBinExpr* cexpr = std::get<NodeBinExpr*>(args->var);
                        if(std::holds_alternative<NodeBinExprArgs*>(cexpr->var)) {
                            std::vector<NodeExpr*> pargs = get<NodeBinExprArgs*>(cexpr->var)->args;
                            if(pargs.size() != proc.value().params.size()) {
                                gen.GeneratorError(term_call->def, "procedure `" + name + "` excepts " + std::to_string(proc.value().params.size()) + " arguments\nNOTE: but got " + std::to_string(pargs.size()));
                            }
                            for(int i = 0;i < static_cast<int>(pargs.size());++i) {
                                if(gen.type_of_expr(pargs[i]) != proc.value().params[i].second) {
                                    gen.GeneratorError(term_call->def, "procedure `" + name + "`\nexcept type " + dt_to_string(proc.value().params[i].second) + " at " + std::to_string(i) + " argument\nNOTE: but found type " + dt_to_string(gen.type_of_expr(pargs[i])));
                                }
                            }
                            stack_allign += pargs.size();
                        }
                    } else {
                        stack_allign++;
                    }
                } else {
                    if(proc.value().params.size() != 0U) {
                        gen.GeneratorError(term_call->def, "procedure `" + name + "` excepts " + std::to_string(proc.value().params.size()) + " args\nNOTE: but got 0");
                    }
                }
                if(term_call->args.has_value()) {
                    gen.gen_expr(term_call->args.value());
                }
                gen.m_output << "    call " << name << "\n";
                if(stack_allign != 0) {
                    gen.m_output << "    add esp, " << stack_allign * 4 << "\n";
                }
                if(proc.value().rettype == DataType::_void) {
                    gen.GeneratorError(term_call->def, "using void function as expression");
                }
                gen.m_output << "    push eax\n";
            }
        };
        TermVisitor visitor({ .gen = *this , .lvalue = lvalue });
        std::visit(visitor, term->var);
    }

    void gen_bin_expr(const NodeBinExpr* bin_expr)
    {
        struct BinExprVisitor {
            Generator& gen;

            void operator()(const NodeBinExprSub* sub) const
            {
                gen.gen_expr(sub->rhs);
                gen.gen_expr(sub->lhs);
                gen.pop("eax");
                gen.pop("ebx");
                gen.m_output << "    sub eax, ebx\n";
                gen.push("eax");
            }

            void operator()(const NodeBinExprAdd* add) const
            {
                gen.gen_expr(add->rhs);
                gen.gen_expr(add->lhs);
                gen.pop("eax");
                gen.pop("ebx");
                gen.m_output << "    add eax, ebx\n";
                gen.push("eax");
            }

            void operator()(const NodeBinExprMulti* multi) const
            {
                gen.gen_expr(multi->rhs);
                gen.gen_expr(multi->lhs);
                gen.pop("eax");
                gen.pop("ebx");
                gen.m_output << "    mul ebx\n";
                gen.push("eax");
            }

            void operator()(const NodeBinExprDiv* div) const
            {
                gen.gen_expr(div->rhs);
                gen.gen_expr(div->lhs);
                gen.pop("eax");
                gen.pop("ebx");
                gen.m_output << "    mov edx, 0\n";
                gen.m_output << "    idiv ebx\n";
                gen.push("eax");
                gen.m_output << "    mov edx, ecx\n";
            }

            void operator()(const NodeBinExprEqEq* eqeq) const
            {
                gen.gen_expr(eqeq->rhs);
                gen.gen_expr(eqeq->lhs);
                gen.m_output << "    mov edx, 0\n";
                gen.m_output << "    mov ecx, 1\n";
                gen.m_output << "    pop ebx\n";
                gen.m_output << "    pop eax\n";
                gen.m_output << "    cmp eax, ebx\n";
                gen.m_output << "    cmove edx, ecx\n";
                gen.m_output << "    push edx\n";
            }

            void operator()(const NodeBinExprNotEq* nq) const
            {
                gen.gen_expr(nq->rhs);
                gen.gen_expr(nq->lhs);
                gen.m_output << "    mov edx, 0\n";
                gen.m_output << "    mov ecx, 1\n";
                gen.m_output << "    pop ebx\n";
                gen.m_output << "    pop eax\n";
                gen.m_output << "    cmp eax, ebx\n";
                gen.m_output << "    cmovne edx, ecx\n";
                gen.m_output << "    push edx\n";
            }

            void operator()(const NodeBinExprLess* less) const
            {
                gen.gen_expr(less->rhs);
                gen.gen_expr(less->lhs);
                gen.m_output << "    mov edx, 0\n";
                gen.m_output << "    mov ecx, 1\n";
                gen.m_output << "    pop ebx\n";
                gen.m_output << "    pop eax\n";
                gen.m_output << "    cmp eax, ebx\n";
                gen.m_output << "    cmovae edx, ecx\n";
                gen.m_output << "    push edx\n";
            }

            void operator()(const NodeBinExprAbove* above) const
            {
                gen.gen_expr(above->lhs);
                // DON'T COPY PASTE
                // IN > FIRST GENERATE LEFT OPERAND
                gen.gen_expr(above->rhs);
                gen.m_output << "    mov edx, 0\n";
                gen.m_output << "    mov ecx, 1\n";
                gen.m_output << "    pop ebx\n";
                gen.m_output << "    pop eax\n";
                gen.m_output << "    cmp eax, ebx\n";
                gen.m_output << "    cmova edx, ecx\n";
                gen.m_output << "    push edx\n";
            }

            void operator()(const NodeBinExprArgs* args) const
            {
                for(int i = 0;i < static_cast<int>(args->args.size());++i) {
                    gen.gen_expr(args->args[i]);
                }
            }
        };
        std::string bin_str = "";
        if(std::holds_alternative<NodeBinExprAdd*>(bin_expr->var)) {
            bin_str = "+";
        } else if(std::holds_alternative<NodeBinExprSub*>(bin_expr->var)) {
            bin_str = "-";
        } else if(std::holds_alternative<NodeBinExprMulti*>(bin_expr->var)) {
            bin_str = "*";
        } else if(std::holds_alternative<NodeBinExprDiv*>(bin_expr->var)) {
            bin_str = "/";
        } else if(std::holds_alternative<NodeBinExprEqEq*>(bin_expr->var)) {
            bin_str = "==";
        } else if(std::holds_alternative<NodeBinExprNotEq*>(bin_expr->var)) {
            bin_str = "!=";
        } else if(std::holds_alternative<NodeBinExprLess*>(bin_expr->var)) {
            bin_str = "<";
        } else if(std::holds_alternative<NodeBinExprAbove*>(bin_expr->var)) {
            bin_str = ">";
        }
        if(!std::holds_alternative<NodeBinExprArgs*>(bin_expr->var)) {
            typecheck_bin_expr_err(bin_expr, bin_str);
        }
        BinExprVisitor visitor { .gen = *this };
        std::visit(visitor, bin_expr->var);
    }

    void gen_expr(const NodeExpr* expr, bool lvalue = false)
    {
        struct ExprVisitor {
            Generator& gen;
            bool lvalue;

            void operator()(const NodeTerm* term) const
            {
                gen.gen_term(term, lvalue);
            }

            void operator()(const NodeBinExpr* bin_expr) const
            {
                gen.gen_bin_expr(bin_expr);
            }
        };

        ExprVisitor visitor { .gen = *this , .lvalue = lvalue };
        std::visit(visitor, expr->var);
    }

    void gen_scope(const NodeScope* scope)
    {
        begin_scope();
        for (const NodeStmt* stmt : scope->stmts) {
            gen_stmt(stmt);
        }
        end_scope();
    }

    void gen_scope_fsz(const NodeScope* scope, const int psizes = 0)
    {
        int fsz = 0;
        for (const NodeStmt* stmt : scope->stmts) {
            if(holds_alternative<NodeStmtLet*>(stmt->var)) {
                fsz += 1;
            }
        }
        begin_scope_fsz(fsz + psizes);
        for (const NodeStmt* stmt : scope->stmts) {
            gen_stmt(stmt);
        }
        end_scope_fsz(fsz + psizes);
    }

    void create_var(const std::string name, NodeExpr* value, Token where) {
        std::optional<Var> ivar = var_lookup(name);
        if(ivar.has_value()) {
            GeneratorError(where, "name `" + name + "` already in use");
        }
        DataType vartype = type_of_expr(value);
        m_vars.push_back({ .name = name, .stack_loc = (m_vars.size() + 1) * 4 , .type = vartype });
        gen_expr(value);
        m_output << "    pop ecx\n";
        m_output << "    mov dword [ebp-" << m_vars.size() * 4 << "], ecx\n";
    }

    void create_var_va(const std::string name, DataType type, Token where) {
        std::optional<Var> ivar = var_lookup(name);
        if(ivar.has_value()) {
            GeneratorError(where, "name `" + name + "` already in use");
        }
        m_vars.push_back({ .name = name, .stack_loc = (m_vars.size() + 1) * 4 , .type = type });
    }

    void gen_if_pred(const NodeIfPred* pred, const std::string& end_label)
    {
        struct PredVisitor {
            Generator& gen;
            const std::string& end_label;

            void operator()(const NodeIfPredElif* elif) const
            {
                gen.gen_expr(elif->expr);
                gen.pop("eax");
                const std::string label = gen.create_label();
                gen.m_output << "    test eax, eax\n";
                gen.m_output << "    jz " << label << "\n";
                gen.gen_scope(elif->scope);
                gen.m_output << "    jmp " << end_label << "\n";
                if (elif->pred.has_value()) {
                    gen.m_output << label << ":\n";
                    gen.gen_if_pred(elif->pred.value(), end_label);
                }
            }

            void operator()(const NodeIfPredElse* else_) const
            {
                gen.gen_scope(else_->scope);
            }
        };

        PredVisitor visitor { .gen = *this, .end_label = end_label };
        std::visit(visitor, pred->var);
    }

    void gen_stmt(const NodeStmt* stmt)
    {
        struct StmtVisitor {
            Generator& gen;

            void operator()(const NodeStmtExit* stmt_exit) const
            {
                DataType etype = gen.type_of_expr(stmt_exit->expr);
                if(etype != DataType::_int) {
                    gen.GeneratorError(stmt_exit->def, "`exit` except type `int`\nNOTE: but got type " + dt_to_string(etype));
                }
                gen.gen_expr(stmt_exit->expr);
                gen.m_output << "    call ExitProcess@4\n";
            }

            void operator()(const NodeStmtPrint* stmt_print) const
            {
                DataType etype = gen.type_of_expr(stmt_print->expr);
                if(etype != DataType::_int && etype != DataType::ptr) {
                    gen.GeneratorError(stmt_print->def, "`print` except types `int` or `print`\nNOTE: but found " + dt_to_string(etype));
                }
                if(etype == DataType::_int) {
                    gen.gen_expr(stmt_print->expr);
                    gen.m_output << "    push numfmt\n";
                    gen.m_output << "    call printf\n";
                    gen.m_output << "    add esp, 8\n";
                } else if(etype == DataType::ptr) {
                    gen.gen_expr(stmt_print->expr);
                    gen.m_output << "    call printf\n";
                    gen.m_output << "    add esp, 4\n";
                } else {
                    assert(false);
                }
            }

            void operator()(const NodeStmtProc* stmt_proc)
            {
                std::optional<Procedure> proc = gen.proc_lookup(stmt_proc->name);
                if(proc.has_value()) {
                    return;
                }
                size_t fsz = 0;
                for (const NodeStmt* stmt : stmt_proc->scope->stmts) {
                    if(holds_alternative<NodeStmtLet*>(stmt->var)) {
                        fsz += 1;
                    }
                }
                gen.m_procs.push_back({ .name = stmt_proc->name , .params = stmt_proc->params , .rettype = stmt_proc->rettype, .stack_allign = stmt_proc->params.size() + fsz});
                for(int i = 0;i < static_cast<int>(stmt_proc->params.size());++i) {
                    gen.create_var_va(stmt_proc->params[i].first, stmt_proc->params[i].second, stmt_proc->def);
                }
                gen.m_output << stmt_proc->name << ":\n";
                gen.m_output << "    push ebp\n";
                gen.m_output << "    mov ebp, esp\n";
                if(static_cast<int>(stmt_proc->params.size()) != 0U) {
                    int rev_i = 0;
                    for(int i = static_cast<int>(stmt_proc->params.size()) - 1;i > -1;--i, rev_i++) {
                        gen.m_output << "    mov edx, dword [esp+" << i * 4 + 8 << "]\n";
                        gen.m_output << "    mov dword [ebp-" << rev_i * 4 + 4 << "], edx\n";
                    }
                }
                gen.m_cur_proc = gen.m_procs[gen.m_procs.size() - 1];
                gen.gen_scope_fsz(stmt_proc->scope, static_cast<int>(stmt_proc->params.size()));
                gen.m_cur_proc = std::nullopt;
                gen.m_output << "    pop ebp\n";
                gen.m_output << "    ret\n\n";
                gen.m_vars.clear();
            }

            void operator()(const NodeStmtReturn* stmt_return) const
            {
                std::optional<Procedure> cproc = gen.m_cur_proc;
                if(!cproc.has_value()) {
                    gen.GeneratorError(stmt_return->def, "return without procedure");
                }
                DataType rettype = cproc.value().rettype;
                if(rettype == DataType::_void) {
                    gen.GeneratorError(stmt_return->def, "return from void procedure with value");
                }
                if(gen.type_of_expr(stmt_return->expr) != rettype) {
                    gen.GeneratorError(stmt_return->def, "procedure `" + cproc.value().name + "` at return except type " + dt_to_string(rettype) + "\nNOTE: but got type " + dt_to_string(gen.type_of_expr(stmt_return->expr)));
                }
                gen.gen_expr(stmt_return->expr);
                gen.pop("eax");
                gen.end_scope_fsz(cproc.value().stack_allign);
                gen.m_output << "    pop ebp\n";
                gen.m_output << "    ret\n";
            }

            void operator()(const NodeStmtLet* stmt_let) const
            {
                gen.create_var(stmt_let->ident.value.value(), stmt_let->expr, stmt_let->ident);
            }

            void operator()(const NodeStmtAssign* stmt_assign) const
            {
                NodeExpr* lvalue = stmt_assign->lvalue;
                if(std::holds_alternative<NodeTerm*>(lvalue->var)) {
                    NodeTerm* lvterm = std::get<NodeTerm*>(lvalue->var);
                    if(std::holds_alternative<NodeTermIdent*>(lvterm->var)) {
                        NodeTermIdent* lvident = std::get<NodeTermIdent*>(lvterm->var);
                        std::string name = lvident->ident.value.value();
                        std::optional<Var> var = gen.var_lookup(name);
                        if(!var.has_value()) {
                            gen.GeneratorError(stmt_assign->def, "unkown variable `" + name + "` at assignment");
                        }
                        gen.gen_expr(stmt_assign->expr);
                        gen.pop("edx");
                        gen.m_output << "    mov dword [ebp-" << var.value().stack_loc << "], edx\n";
                        return;
                    }
                }
                gen.gen_expr(stmt_assign->lvalue, true);
                gen.gen_expr(stmt_assign->expr);
                gen.pop("ecx");
                gen.pop("edx");
                gen.m_output << "    mov dword [edx], ecx\n";
            }

            void operator()(const NodeStmtCall* stmt_call) const
            {
                const std::string name = stmt_call->def.value.value();
                std::optional<Procedure> proc = gen.proc_lookup(name);
                if(!proc.has_value()) {
                    gen.GeneratorError(stmt_call->def, "unkown procedure `" + name + "`");
                }
                size_t stack_allign = 0;
                if(stmt_call->args.has_value()) {
                    if(proc.value().params.size() == 0) {
                        gen.GeneratorError(stmt_call->def, "procedure `" + name + "` don't excepts any arguments");
                    }
                    NodeExpr* targs = stmt_call->args.value();
                    NodeExpr* args = std::get<NodeTermParen*>(std::get<NodeTerm*>(targs->var)->var)->expr;
                    if(std::holds_alternative<NodeBinExpr*>(args->var)) {
                        NodeBinExpr* cexpr = std::get<NodeBinExpr*>(args->var);
                        if(std::holds_alternative<NodeBinExprArgs*>(cexpr->var)) {
                            std::vector<NodeExpr*> pargs = get<NodeBinExprArgs*>(cexpr->var)->args;
                            if(pargs.size() != proc.value().params.size()) {
                                gen.GeneratorError(stmt_call->def, "procedure `" + name + "` excepts " + std::to_string(proc.value().params.size()) + " arguments\nNOTE: but got " + std::to_string(pargs.size()));
                            }
                            for(int i = 0;i < static_cast<int>(pargs.size());++i) {
                                if(gen.type_of_expr(pargs[i]) != proc.value().params[i].second) {
                                    gen.GeneratorError(stmt_call->def, "procedure `" + name + "`\nexcept type " + dt_to_string(proc.value().params[i].second) + " at " + std::to_string(i) + " argument\nNOTE: but found type " + dt_to_string(gen.type_of_expr(pargs[i])));
                                }
                            }
                            stack_allign += pargs.size();
                        }
                    } else {
                        stack_allign++;
                    }
                } else {
                    if(proc.value().params.size() != 0U) {
                        gen.GeneratorError(stmt_call->def, "procedure `" + name + "` excepts " + std::to_string(proc.value().params.size()) + " args\nNOTE: but got 0");
                    }
                }
                if(stmt_call->args.has_value()) {
                    gen.gen_expr(stmt_call->args.value());
                }
                gen.m_output << "    call " << name << "\n";
                if(stack_allign != 0) {
                    gen.m_output << "    add esp, " << stack_allign * 4 << "\n";
                }
            }

            void operator()(const NodeScope* scope) const
            {
                gen.gen_scope(scope);
            }

            void operator()(const NodeStmtIf* stmt_if) const
            {
                gen.gen_expr(stmt_if->expr);
                gen.pop("eax");
                const std::string label = gen.create_label();
                gen.m_output << "    test eax, eax\n";
                gen.m_output << "    jz " << label << "\n";
                gen.gen_scope(stmt_if->scope);
                if (stmt_if->pred.has_value()) {
                    const std::string end_label = gen.create_label();
                    gen.m_output << "    jmp " << end_label << "\n";
                    gen.m_output << "    " << label << ":\n";
                    gen.gen_if_pred(stmt_if->pred.value(), end_label);
                    gen.m_output << "    " << end_label << ":\n";
                }
                else {
                    gen.m_output << "    " << label << ":\n";
                }
            }

            void operator()(const NodeStmtWhile* stmt_while) const
            {
                auto preiflab = gen.create_label();
                auto blocklab = gen.create_label();
                auto breaklab = gen.create_label();
                gen.m_output << "    " << preiflab << ":\n";
                gen.gen_expr(stmt_while->expr);
                gen.m_output << "    pop eax\n";
                gen.m_output << "    test eax, eax\n";
                gen.m_output << "    jz " << breaklab << "\n";
                gen.m_output << "    " << blocklab << ":\n";
                gen.gen_scope(stmt_while->scope);
                gen.m_output << "    jmp " << preiflab << "\n";
                gen.m_output << "    " << breaklab << ":\n";
            }

            void operator()(const NodeStmtStore* stmt_store) const
            {
                DataType ptype = gen.type_of_expr(stmt_store->ptr);
                DataType etype = gen.type_of_expr(stmt_store->expr);
                if(ptype != DataType::ptr && etype != DataType::_int) {
                    gen.GeneratorError(stmt_store->def, "store types missmatch\nNOTE: except `ptr`, `int`\nNOTE: but got " + dt_to_string(ptype) + ", " + dt_to_string(etype));
                }
                if(stmt_store->size == 8U) {
                    gen.gen_expr(stmt_store->ptr);
                    gen.gen_expr(stmt_store->expr);
                    gen.m_output << "    pop edx\n";
                    gen.m_output << "    pop ecx\n";
                    gen.m_output << "    mov byte [ecx], dl\n";
                }
                else if(stmt_store->size == 16U) {
                    gen.gen_expr(stmt_store->ptr);
                    gen.gen_expr(stmt_store->expr);
                    gen.m_output << "    pop edx\n";
                    gen.m_output << "    pop ecx\n";
                    gen.m_output << "    mov word [ecx], dx\n";
                }
                else if(stmt_store->size == 32U) {
                    gen.gen_expr(stmt_store->ptr);
                    gen.gen_expr(stmt_store->expr);
                    gen.m_output << "    pop edx\n";
                    gen.m_output << "    pop ecx\n";
                    gen.m_output << "    mov dword [ecx], edx\n";
                } else {
                    assert(false); // unreacheable
                }
            }
        };

        StmtVisitor visitor { .gen = *this };
        std::visit(visitor, stmt->var);
    }

    [[nodiscard]] std::string gen_prog()
    {
        std::stringstream result;
        result << "section .text\n\n";
        result << "global main\n\n";
        yforeach(m_cexterns) {
            result << "extern " << m_cexterns[i] << "\n";
        }
        result << "\n";

        for (const NodeStmt* stmt : m_prog.stmts) {
            gen_stmt(stmt);
        }

        m_output << "\nsection .data\n";
        m_output << "    numfmt: db \"%d\", 0xa, 0x0\n";
        m_output << "    strfmt: db \"%s\", 0x0\n";
        for(int i = 0;i < static_cast<int>(m_strings.size());++i) {
            String& cur_s = m_strings[i];
            m_output << "    str_" << static_cast<int>(cur_s.index) << ": db ";
            std::stringstream hexstr;
            for(int j = 0;j < static_cast<int>(cur_s.value.length());++j) {
                hexstr << "0x" << std::hex << static_cast<int>(cur_s.value[j]) << ", ";
            }
            m_output << hexstr.str();
            hexstr.clear();
            m_output << "0x0\n";
        }
        result << m_output.str();
        return result.str();
    }

private:
    void push(const std::string& reg)
    {
        m_output << "    push " << reg << "\n";
    }

    void pop(const std::string& reg)
    {
        m_output << "    pop " << reg << "\n";
    }

    void begin_scope() {}

    void end_scope() {}

    void begin_scope_fsz(int fsz)
    {
        if(fsz != 0) {
            m_output << "    sub esp, " << fsz * 4 << "\n";
        }
        m_scopes.push_back(m_vars.size());
    }

    void end_scope_fsz(int fsz)
    {
        if(fsz != 0) {
            m_output << "    add esp, " << fsz * 4 << "\n";
        }
        m_scopes.pop_back();
    }

    std::string create_label()
    {
        std::stringstream ss;
        ss << "L" << m_label_count++;
        return ss.str();
    }

    const NodeProg m_prog;
    std::stringstream        m_output   {};
    std::vector<Var>         m_vars     {};
    std::vector<String>      m_strings  {};
    std::vector<size_t>      m_scopes   {};
    std::vector<Procedure>   m_procs    {};
    std::optional<Procedure> m_cur_proc {};
    std::vector<std::string> m_cexterns = {
        "printf",
        "ExitProcess@4"
    };
    int m_label_count = 0;
};