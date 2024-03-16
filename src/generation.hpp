#pragma once

#include <algorithm>
#include <cassert>

#include "parser.hpp"

class Generator {
public:
    struct Var {
        std::string name;
        size_t stack_loc;
        DataType type;
    };
    struct String {
        std::string value;
        size_t index;
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
                return DataType::string;
            }
            if(std::holds_alternative<NodeTermParen*>(term->var)) {
                return type_of_expr(std::get<NodeTermParen*>(term->var)->expr);
            }
            if(std::holds_alternative<NodeTermIdent*>(term->var)) {
                std::optional<Var> svar = var_lookup(std::get<NodeTermIdent*>(term->var)->ident.value.value());
                if(!svar.has_value()) {
                    return DataType::_int;
                }
                return svar.value().type;
            }
        }
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
        }
        assert(false);
    }

    bool typecheck_bin_expr(const NodeBinExpr* expr) {
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
        } else {
            assert(false);
        }
    }

    void typecheck_bin_expr_err(const NodeBinExpr* expr, std::string IRexpr) {
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
            } else {
                assert(false);
            }
            GeneratorError(expr->def, "can't use `" + IRexpr + "` for types " + dt_to_string(ltype) + " and " + dt_to_string(rtype));
        }
    }

    void gen_term(const NodeTerm* term)
    {
        struct TermVisitor {
            Generator& gen;

            void operator()(const NodeTermIntLit* term_int_lit) const
            {
                gen.push(term_int_lit->int_lit.value.value());
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
                std::stringstream offset;
                offset << "dword [ebp-" << it.stack_loc << "]";
                gen.push(offset.str());
            }

            void operator()(const NodeTermParen* term_paren) const
            {
                gen.gen_expr(term_paren->expr);
            }
        };
        TermVisitor visitor({ .gen = *this });
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
        }
        typecheck_bin_expr_err(bin_expr, bin_str);

        BinExprVisitor visitor { .gen = *this };
        std::visit(visitor, bin_expr->var);
    }

    void gen_expr(const NodeExpr* expr)
    {
        struct ExprVisitor {
            Generator& gen;

            void operator()(const NodeTerm* term) const
            {
                gen.gen_term(term);
            }

            void operator()(const NodeBinExpr* bin_expr) const
            {
                gen.gen_bin_expr(bin_expr);
            }
        };

        ExprVisitor visitor { .gen = *this };
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

    void gen_scope_fsz(const NodeScope* scope)
    {
        int fsz = 0;
        for (const NodeStmt* stmt : scope->stmts) {
            if(holds_alternative<NodeStmtLet*>(stmt->var)) {
                fsz += 1;
            }
        }
        begin_scope_fsz(fsz);
        for (const NodeStmt* stmt : scope->stmts) {
            gen_stmt(stmt);
        }
        end_scope_fsz(fsz);
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
                if(etype != DataType::_int && etype != DataType::string) {
                    gen.GeneratorError(stmt_print->def, "`print` except types `int` or `print`\nNOTE: but found " + dt_to_string(etype));
                }
                if(etype == DataType::_int) {
                    gen.gen_expr(stmt_print->expr);
                    gen.m_output << "    push numfmt\n";
                    gen.m_output << "    call printf\n";
                    gen.m_output << "    add esp, 8\n";
                } else if(etype == DataType::string) {
                    gen.gen_expr(stmt_print->expr);
                    gen.m_output << "    call printf\n";
                    gen.m_output << "    add esp, 4\n";
                } else {
                    assert(false);
                }
            }

            void operator()(const NodeStmtProc* stmt_proc) const
            {
                gen.m_output << stmt_proc->name << ":\n";
                gen.m_output << "    push ebp\n";
                gen.m_output << "    mov ebp, esp\n";
                gen.gen_scope_fsz(stmt_proc->scope);
                gen.m_output << "    pop ebp\n";
                gen.m_output << "    ret\n";
            }

            void operator()(const NodeStmtLet* stmt_let) const
            {
                bool finded = false;
                for(Var var : gen.m_vars) {
                    if(var.name == stmt_let->ident.value.value()) {
                        finded = true;
                    }
                }
                if(finded) {
                    gen.GeneratorError(stmt_let->ident, "name `" + stmt_let->ident.value.value() + "` already in use");
                }
                DataType vartype = gen.type_of_expr(stmt_let->expr);
                gen.m_vars.push_back({ .name = stmt_let->ident.value.value(), .stack_loc = (gen.m_vars.size() + 1) * 4 , .type = vartype });
                gen.gen_expr(stmt_let->expr);
                gen.m_output << "    pop ecx\n";
                gen.m_output << "    mov dword [ebp-" << gen.m_vars.size() * 4 << "], ecx\n";
            }

            void operator()(const NodeStmtAssign* stmt_assign) const
            {
                std::optional<Var> it = gen.var_lookup(stmt_assign->ident.value.value());
                if(!it.has_value()) {
                    gen.GeneratorError(stmt_assign->ident, "unkown word `" + stmt_assign->ident.value.value() + "`");
                }
                DataType etype = gen.type_of_expr(stmt_assign->expr);
                if(it.value().type != etype) {
                    gen.GeneratorError(stmt_assign->ident, "missmatch in types\nNOTE: variable `" + stmt_assign->ident.value.value() + "` has type " + dt_to_string(it.value().type) + " but got type " + dt_to_string(etype));
                }
                gen.gen_expr(stmt_assign->expr);
                gen.pop("ecx");
                gen.m_output << "    mov [ebp-" << it.value().stack_loc << "], ecx\n";
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
        };

        StmtVisitor visitor { .gen = *this };
        std::visit(visitor, stmt->var);
    }

    [[nodiscard]] std::string gen_prog()
    {
        std::stringstream result;
        result << "section .text\n\n";
        result << "global main\n\n";
        result << "extern printf\n";
        result << "extern ExitProcess@4\n\n";

        for (const NodeStmt* stmt : m_prog.stmts) {
            gen_stmt(stmt);
        }

        m_output << "\n\nsection .data\n";
        m_output << "    numfmt: db \"%d\", 0\n";
        m_output << "    strfmt: db \"%s\", 0\n";
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
        m_output << "    sub esp, " << fsz * 4 << "\n";
        m_scopes.push_back(m_vars.size());
    }

    void end_scope_fsz(int fsz)
    {
        m_output << "    add esp, " << fsz * 4 << "\n";
        m_scopes.pop_back();
    }

    std::string create_label()
    {
        std::stringstream ss;
        ss << "L" << m_label_count++;
        return ss.str();
    }

    const NodeProg m_prog;
    std::stringstream m_output;
    std::vector<Var> m_vars {};
    std::vector<String> m_strings {};
    std::vector<size_t> m_scopes {};
    int m_label_count = 0;
};