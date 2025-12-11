#pragma once

class TypeSubstitutor {
public:

    GMap<GString, DataType> m_templates;

    explicit TypeSubstitutor(const GMap<GString, DataType>& temps) {
        m_templates = temps;
    }

    DataType substitute_type(const DataType& type) {
        DataType result(type);

        if(!result.node->generics.empty()) {
            for(size_t i = 0;i < result.node->generics.size();i++) {
                result.node->generics[i] =  substitute_type(result.node->generics[i]);
            }
        }
        
        if(!result.is_object()) return result;

        const GString& object_name = result.getobjectname();

        auto search = m_templates.find(object_name);

        if(search == m_templates.end()) return result;

         const DataType& template_source = search->second;

        DataType replacement(template_source.root());
        
        replacement.node->generics = template_source.node->generics;

        replacement.root().ptrlvl += result.root().ptrlvl;

        return replacement;
    }

    void substitute_term(const NodeTerm* term)
    {
        if(term == nullptr) return;

        struct TermVisitor {
            TypeSubstitutor& substitutor;

            void operator()([[maybe_unused]] const NodeTermIntLit* term_int_lit) const {}
            void operator()([[maybe_unused]] const NodeTermCol* term_col) const {}
            void operator()([[maybe_unused]] const NodeTermLine* term_line) const {}
            void operator()([[maybe_unused]] const NodeTermPop* term_pop) const {}
            void operator()([[maybe_unused]] const NodeTermFile* term_file) const {}
            void operator()([[maybe_unused]] const NodeTermCtMdefined* term_mdef) const {}
            void operator()([[maybe_unused]] const NodeTermStrLit* term_str_lit) const {}
            void operator()([[maybe_unused]] const NodeTermIdent* term_ident) const {}
            void operator()([[maybe_unused]] const NodeTermNmIdent* term_ident) const {}

            void operator()(NodeTermType* tp) const {
                tp->type = substitutor.substitute_type(tp->type);
            }

            void operator()(NodeTermSizeof* term_sizeof) const {
                term_sizeof->type = substitutor.substitute_type(term_sizeof->type);
                if(term_sizeof->expr.has_value()) substitutor.substitute_expr(term_sizeof->expr.value());
            }

            void operator()(NodeTermCast* term_cast) const {
                term_cast->type = substitutor.substitute_type(term_cast->type);
                if(term_cast->expr) substitutor.substitute_expr(term_cast->expr);
            }

            void operator()(NodeTermTypeid* term_typeid) const {
                if(term_typeid->ptype.has_value()) term_typeid->ptype = substitutor.substitute_type(term_typeid->ptype.value());
                if(term_typeid->expr) substitutor.substitute_expr(term_typeid->expr);
            }

            void operator()(NodeTermNmCall* term_call) const {
                for(size_t i = 0;i < term_call->targs.size();i++) {
                    term_call->targs[i] = substitutor.substitute_type(term_call->targs[i]);
                }
                if(term_call->args.has_value()) substitutor.substitute_expr(term_call->args.value());
            }

            void operator()(NodeTermCall* term_call) const {
                for(size_t i = 0;i < term_call->targs.size();i++) {
                    term_call->targs[i] = substitutor.substitute_type(term_call->targs[i]);
                }
                if(term_call->args.has_value()) substitutor.substitute_expr(term_call->args.value());
            }

            void operator()(NodeTermMtCall* term_call) const {
                for(size_t i = 0;i < term_call->targs.size();i++) {
                    term_call->targs[i] = substitutor.substitute_type(term_call->targs[i]);
                }
                if(term_call->args.has_value()) substitutor.substitute_expr(term_call->args.value());
                substitutor.substitute_expr(term_call->mt);
            }

            void operator()(NodeTermCtEval* term_eval) const {
                substitutor.substitute_expr(term_eval->expr);
            }

            void operator()(NodeTermExprStmt* term_stmt) const {
                substitutor.substitute_scope(term_stmt->scope);
                substitutor.substitute_expr(term_stmt->expr);
            }

            void operator()(NodeTermUnref* term_unref) const {
                substitutor.substitute_expr(term_unref->expr);
            }

            void operator()(NodeTermCastTo* term_cast_to) const {
                substitutor.substitute_expr(term_cast_to->to);
                substitutor.substitute_expr(term_cast_to->expr);
            }

            void operator()(NodeTermRd* term_rd) const {
                substitutor.substitute_expr(term_rd->expr);
            }

            void operator()(NodeTermAmpersand* term_amp) const {
                substitutor.substitute_expr(term_amp->expr);
            }

            void operator()(NodeTermDrvalue* term_drval) const {
                substitutor.substitute_expr(term_drval->expr);
            }

            void operator()(NodeTermParen* term_paren) const {
                substitutor.substitute_expr(term_paren->expr);
            }
        };

        TermVisitor visitor{ *this };
        return std::visit(visitor, term->var);
    }

    void substitute_bin_expr(const NodeBinExpr* bin_expr)
    {
        if(bin_expr == nullptr) return;
        struct BinExprVisitor {
            TypeSubstitutor& substitutor;
            const NodeBinExpr* base;

            void operator()(const NodeBinExprSub* sub) const {
                substitutor.substitute_expr(sub->lhs);
                substitutor.substitute_expr(sub->rhs);
            }

            void operator()(const NodeBinExprAdd* add) const {
                substitutor.substitute_expr(add->lhs);
                substitutor.substitute_expr(add->rhs);
            }

            void operator()(const NodeBinExprMulti* multi) const {
                substitutor.substitute_expr(multi->lhs);
                substitutor.substitute_expr(multi->rhs);
            }

            void operator()(const NodeBinExprDiv* div) const {
                substitutor.substitute_expr(div->lhs);
                substitutor.substitute_expr(div->rhs);
            }

            void operator()(const NodeBinExprShl* shl) const {
                substitutor.substitute_expr(shl->lhs);
                substitutor.substitute_expr(shl->rhs);
            }

            void operator()(const NodeBinExprShr* shr) const {
                substitutor.substitute_expr(shr->lhs);
                substitutor.substitute_expr(shr->rhs);
            }

            void operator()(const NodeBinExprMod* md) const {
                substitutor.substitute_expr(md->lhs);
                substitutor.substitute_expr(md->rhs);
            }

            void operator()(const NodeBinExprEqEq* eqeq) const {
                substitutor.substitute_expr(eqeq->lhs);
                substitutor.substitute_expr(eqeq->rhs);
            }

            void operator()(const NodeBinExprNotEq* nq) const {
                substitutor.substitute_expr(nq->lhs);
                substitutor.substitute_expr(nq->rhs);
            }

            void operator()(const NodeBinExprLess* less) const {
                substitutor.substitute_expr(less->lhs);
                substitutor.substitute_expr(less->rhs);
            }

            void operator()(const NodeBinExprAnd* band) const {
                substitutor.substitute_expr(band->lhs);
                substitutor.substitute_expr(band->rhs);
            }

            void operator()(const NodeBinExprOr* bor) const {
                substitutor.substitute_expr(bor->lhs);
                substitutor.substitute_expr(bor->rhs);
            }

            void operator()(const NodeBinExprAbove* above) const {
                substitutor.substitute_expr(above->lhs);
                substitutor.substitute_expr(above->rhs);
            }

            void operator()(const NodeBinExprDot* dot) const {
                substitutor.substitute_expr(dot->lhs);
                substitutor.substitute_expr(dot->rhs);
            }

            void operator()(NodeBinExprArgs* args) const {
                for(NodeExpr* arg : args->args) {
                    substitutor.substitute_expr(arg);
                }
            }
            void operator()(const NodeBinExprIndex* idx) const {
                substitutor.substitute_expr(idx->lhs);
                substitutor.substitute_expr(idx->rhs);
            }
        };

        BinExprVisitor visitor{ *this , bin_expr };
        return std::visit(visitor, bin_expr->var);
    }

    void substitute_expr(const NodeExpr* expr)
    {
        if(expr == nullptr) return;
        struct ExprVisitor {
            TypeSubstitutor& substitutor;

            void operator()(const NodeTerm* term) const
            {
                substitutor.substitute_term(term);
            }

            void operator()(const NodeBinExpr* bin_expr) const
            {
                substitutor.substitute_bin_expr(bin_expr);
            }
        };

        ExprVisitor visitor{ *this };
        return std::visit(visitor, expr->var);
    }

    void substitute_scope(const NodeScope* scope)
    {
        if(scope == nullptr) return;

        for(const NodeStmt* stmt : scope->stmts) {
            substitute_stmt(stmt);
        }
    }

    void substitute_if_pred(const NodeIfPred* pred)
    {
        if(pred == nullptr) return;

        struct PredVisitor {
            TypeSubstitutor& substitutor;

            void operator()(const NodeIfPredElif* elif) const
            {
                substitutor.substitute_expr(elif->expr);
                substitutor.substitute_scope(elif->scope);
                if(elif->pred.has_value()) substitutor.substitute_if_pred(elif->pred.value());
            }

            void operator()(const NodeIfPredElse* else_) const
            {
                substitutor.substitute_scope(else_->scope);
            }
        };

        PredVisitor visitor{ *this };
        return std::visit(visitor, pred->var);
    }

    void substitute_stmt(const NodeStmt* stmt)
    {
        if(stmt == nullptr) return;

        struct StmtVisitor {
            TypeSubstitutor& substitutor;

            void operator()([[maybe_unused]] const NodeStmtBreak* stmt_break) const {}
            void operator()([[maybe_unused]] const NodeStmtAsm* stmt_asm) const {}
            void operator()([[maybe_unused]] const NodeStmtCextern* stmt_cextern) const {}
            void operator()([[maybe_unused]] const NodeStmtEnum* stmt_enum) const {}

            void operator()(NodeStmtExit* stmt_exit) const
            {
                substitutor.substitute_expr(stmt_exit->expr);
            }

            void operator()(NodeStmtProc* stmt_proc)
            {
                stmt_proc->rettype = substitutor.substitute_type(stmt_proc->rettype);
                for(size_t i = 0;i < stmt_proc->params.size();i++) {
                    stmt_proc->params[i].second = substitutor.substitute_type(stmt_proc->params[i].second);
                }
                for(size_t i = 0;i < stmt_proc->constraints.size();i++) {
                    stmt_proc->constraints[i].iface_type = substitutor.substitute_type(stmt_proc->constraints[i].iface_type);
                }
                substitutor.substitute_scope(stmt_proc->scope);
            }

            void operator()(NodeStmtReturn* stmt_return) const
            {
                if(stmt_return->expr.has_value()) substitutor.substitute_expr(stmt_return->expr.value());
            }

            void operator()(NodeStmtLet* stmt_let) const
            {
                if(stmt_let->type.has_value()) stmt_let->type = substitutor.substitute_type(stmt_let->type.value());
                if(stmt_let->expr.has_value()) substitutor.substitute_expr(stmt_let->expr.value());
            }

            void operator()(NodeStmtCompileTimeIf* stmt_ctif) const
            {
                substitutor.substitute_expr(stmt_ctif->condition);
                substitutor.substitute_scope(stmt_ctif->_if);
                if(stmt_ctif->_else.has_value()) substitutor.substitute_scope(stmt_ctif->_else.value());
            }

            void operator()(NodeStmtAssign* stmt_assign) const
            {
                substitutor.substitute_expr(stmt_assign->lvalue);
                substitutor.substitute_expr(stmt_assign->expr);
            }

            void operator()(NodeStmtIncBy* stmt_assign) const
            {
                substitutor.substitute_expr(stmt_assign->lvalue);
                substitutor.substitute_expr(stmt_assign->expr);
            }

            void operator()(NodeStmtDecBy* stmt_assign) const
            {
                substitutor.substitute_expr(stmt_assign->lvalue);
                substitutor.substitute_expr(stmt_assign->expr);
            }

            void operator()(NodeStmtMulBy* stmt_assign) const
            {
                substitutor.substitute_expr(stmt_assign->lvalue);
                substitutor.substitute_expr(stmt_assign->expr);
            }

            void operator()(NodeStmtDivBy* stmt_assign) const
            {
                substitutor.substitute_expr(stmt_assign->lvalue);
                substitutor.substitute_expr(stmt_assign->expr);
            }

            void operator()(NodeStmtCall* stmt_call) const
            {
                if(stmt_call->args.has_value()) substitutor.substitute_expr(stmt_call->args.value());
                for(size_t i = 0;i < stmt_call->targs.size();i++) {
                    stmt_call->targs[i] = substitutor.substitute_type(stmt_call->targs[i]);
                }
            }

            void operator()(NodeScope* scope) const
            {
                substitutor.substitute_scope(scope);
            }

            void operator()(NodeStmtPushOnStack* stmt_push) const
            {  
                substitutor.substitute_expr(stmt_push->expr);
            }

            void operator()(NodeStmtIf* stmt_if) const
            {
                substitutor.substitute_expr(stmt_if->expr);
                substitutor.substitute_scope(stmt_if->scope);
                if(stmt_if->pred.has_value()) substitutor.substitute_if_pred(stmt_if->pred.value());
            }

            void operator()(NodeStmtWhile* stmt_while) const
            {   
                substitutor.substitute_expr(stmt_while->expr);
                substitutor.substitute_scope(stmt_while->scope);
            }

            void operator()(NodeStmtStore* stmt_store) const
            {
                substitutor.substitute_expr(stmt_store->ptr);
                substitutor.substitute_expr(stmt_store->expr);
            }

            void operator()(NodeStmtBuffer* stmt_buf) const
            {
                substitutor.substitute_expr(stmt_buf->size);
            }

            void operator()(NodeStmtStruct* stmt_struct) const
            {
                for(size_t i = 0;i < stmt_struct->fields.size();i++) {
                    stmt_struct->fields[i].second = substitutor.substitute_type(stmt_struct->fields[i].second);
                }
            }

            void operator()(NodeStmtInterface* stmt_inter) const
            {
                for(size_t i = 0;i < stmt_inter->methods.size();i++) {
                    stmt_inter->methods[i].rettype = substitutor.substitute_type(stmt_inter->methods[i].rettype);
                    for(size_t j = 0;j < stmt_inter->methods[i].params.size();j++) {
                        stmt_inter->methods[i].params[j].second = substitutor.substitute_type(stmt_inter->methods[i].params[j].second);
                    }
                }
            }

            void operator()(NodeStmtOninit* stmt_oninit) const
            {
                substitutor.substitute_scope(stmt_oninit->scope);
            }

            void operator()(NodeStmtStaticAssert* stmt_st) const
            {
                substitutor.substitute_expr(stmt_st->condition);
            }

            void operator()(NodeStmtDelete* stmt_delete) const
            {
                substitutor.substitute_expr(stmt_delete->expr);
            }

            void operator()(NodeStmtRaise* stmt_raise) const
            {
                substitutor.substitute_expr(stmt_raise->expr);
            }

            void operator()(NodeStmtNamespace* stmt_space) const
            {
                substitutor.substitute_scope(stmt_space->scope);
            }

            void operator()(NodeStmtImpl* stmt_impl) const
            {
                substitutor.substitute_scope(stmt_impl->scope);
            }

            void operator()(NodeStmtNmCall* stmt_call) const
            {
                if(stmt_call->args.has_value()) substitutor.substitute_expr(stmt_call->args.value());
                for(size_t i = 0;i < stmt_call->targs.size();i++) {
                    stmt_call->targs[i] = substitutor.substitute_type(stmt_call->targs[i]);
                }
            }

            void operator()(NodeStmtMtCall* stmt_call) const
            {
                if(stmt_call->args.has_value()) substitutor.substitute_expr(stmt_call->args.value());
                for(size_t i = 0;i < stmt_call->targs.size();i++) {
                    stmt_call->targs[i] = substitutor.substitute_type(stmt_call->targs[i]);
                }
                substitutor.substitute_expr(stmt_call->mt);
            }

            void operator()(NodeStmtConst* stmt_const) const
            {
                substitutor.substitute_expr(stmt_const->expr);
            }

            void operator()(NodeStmtTypedef* stmt_tdef) const
            {
                stmt_tdef->type = substitutor.substitute_type(stmt_tdef->type);
            }

            void operator()(NodeStmtTry* stmt_try) const
            { 
                substitutor.substitute_scope(stmt_try->_try);
                substitutor.substitute_scope(stmt_try->_catch);
                stmt_try->type = substitutor.substitute_type(stmt_try->type);
            }
            void operator()(NodeStmtFor* stmt_for) const
            {
                substitutor.substitute_stmt(stmt_for->init);
                substitutor.substitute_expr(stmt_for->cond);
                substitutor.substitute_stmt(stmt_for->step);
                substitutor.substitute_scope(stmt_for->scope);
            }
            void operator()(NodeStmtForeach* stmt_foreach) const
            {
                substitutor.substitute_expr(stmt_foreach->expr);
                substitutor.substitute_scope(stmt_foreach->scope);
            }
        };

        StmtVisitor visitor{ *this };
        return std::visit(visitor, stmt->var);
    }

    void substitute_prog(NodeProg* prog) {
        for(NodeStmt* stmt : prog->stmts) {
            substitute_stmt(stmt);
        }
    }
};