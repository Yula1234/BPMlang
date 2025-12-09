#pragma once

class AstCloner {
public:

	ArenaAllocator* m_allocator = nullptr;

    explicit AstCloner(ArenaAllocator* arena) {
        m_allocator = arena;
    }

    template <typename T>
    remove_all_const_t<T> copy_simple(T object) {
    	return m_allocator->emplace<std::remove_pointer_t<remove_all_const_t<T>>>(*const_cast<remove_all_const_t<T>>(object));
    }

    NodeTerm* clone_term(const NodeTerm* term)
    {
        if(term == nullptr) {
            return nullptr;
        }

        struct TermVisitor {
            AstCloner& cloner;

            NodeTerm* operator()(const NodeTermIntLit* term_int_lit) const {
            	return AstConverter::term(cloner.m_allocator, cloner.copy_simple(term_int_lit));
            }

            NodeTerm* operator()(const NodeTermType* tp) {
            	return AstConverter::term(cloner.m_allocator, cloner.copy_simple(tp));
            }

            NodeTerm* operator()(const NodeTermCol* term_col) const {
                return AstConverter::term(cloner.m_allocator, cloner.copy_simple(term_col));
            }

            NodeTerm* operator()(const NodeTermLine* term_line) const {
                return AstConverter::term(cloner.m_allocator, cloner.copy_simple(term_line));
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermPop* term_pop) const {
                return AstConverter::term(cloner.m_allocator, cloner.m_allocator->emplace<NodeTermPop>());
            }

            NodeTerm* operator()(const NodeTermExprStmt* term_stmt) const {
                NodeTermExprStmt* new_expr = cloner.m_allocator->emplace<NodeTermExprStmt>();
                new_expr->def = term_stmt->def;
                new_expr->scope = cloner.clone_scope(term_stmt->scope);
                new_expr->expr = cloner.clone_expr(term_stmt->expr);
                return AstConverter::term(cloner.m_allocator, new_expr);
            }

            NodeTerm* operator()(const NodeTermFile* term_file) const {
                return AstConverter::term(cloner.m_allocator, cloner.copy_simple(term_file));
            }

            NodeTerm* operator()(const NodeTermCtEval* term_eval) const {
                NodeTermCtEval* new_ct_eval = cloner.m_allocator->emplace<NodeTermCtEval>();
                new_ct_eval->def = term_eval->def;
                new_ct_eval->expr = cloner.clone_expr(term_eval->expr);
                return AstConverter::term(cloner.m_allocator, new_ct_eval);
            }

            NodeTerm* operator()(const NodeTermCtMdefined* term_mdef) const {
                return AstConverter::term(cloner.m_allocator, cloner.copy_simple(term_mdef));
            }

            NodeTerm* operator()(const NodeTermSizeof* term_sizeof) const {
                NodeTermSizeof* new_szof = cloner.m_allocator->emplace<NodeTermSizeof>();
                new_szof->def = term_sizeof->def;
                new_szof->type = term_sizeof->type;
                if(term_sizeof->expr.has_value()) new_szof->expr = cloner.clone_expr(term_sizeof->expr.value());
                else new_szof->expr = std::nullopt;
                return AstConverter::term(cloner.m_allocator, new_szof);
            }

            NodeTerm* operator()(const NodeTermRd* term_rd) const {
                NodeTermRd* new_rd = cloner.m_allocator->emplace<NodeTermRd>();
                new_rd->def = term_rd->def;
                new_rd->size = term_rd->size;
                new_rd->expr = cloner.clone_expr(term_rd->expr);
                return AstConverter::term(cloner.m_allocator, new_rd);
            }

            NodeTerm* operator()(const NodeTermCast* term_cast) const {
                NodeTermCast* new_cast = cloner.m_allocator->emplace<NodeTermCast>();
                new_cast->def = term_cast->def;
                new_cast->type = term_cast->type;
                new_cast->expr = cloner.clone_expr(term_cast->expr);
                return AstConverter::term(cloner.m_allocator, new_cast);
            }

            NodeTerm* operator()(const NodeTermUnref* term_unref) const {
                NodeTermUnref* new_unref = cloner.m_allocator->emplace<NodeTermUnref>();
                new_unref->def = term_unref->def;
                new_unref->expr = cloner.clone_expr(term_unref->expr);
                return AstConverter::term(cloner.m_allocator, new_unref);
            }

            NodeTerm* operator()(const NodeTermCastTo* term_cast_to) const {
                NodeTermCastTo* new_cast_to = cloner.m_allocator->emplace<NodeTermCastTo>();
                new_cast_to->def = term_cast_to->def;
                new_cast_to->to = cloner.clone_expr(term_cast_to->to);
                new_cast_to->expr = cloner.clone_expr(term_cast_to->expr);
                return AstConverter::term(cloner.m_allocator, new_cast_to);
            }

            NodeTerm* operator()(const NodeTermTypeid* term_typeid) const {
                NodeTermTypeid* new_typeid = cloner.m_allocator->emplace<NodeTermTypeid>();
                new_typeid->def = term_typeid->def;
                if(term_typeid->ptype.has_value()) new_typeid->ptype = term_typeid->ptype.value();
                else new_typeid->ptype = std::nullopt;
                new_typeid->expr = cloner.clone_expr(term_typeid->expr);
                return AstConverter::term(cloner.m_allocator, new_typeid);
            }

            NodeTerm* operator()(const NodeTermStrLit* term_str_lit) const {
            	return AstConverter::term(cloner.m_allocator, cloner.copy_simple(term_str_lit));
            }

            NodeTerm* operator()(const NodeTermAmpersand* term_amp) const {
                NodeTermAmpersand* new_amp = cloner.m_allocator->emplace<NodeTermAmpersand>();
                new_amp->def = term_amp->def;
                new_amp->expr = cloner.clone_expr(term_amp->expr);
                return AstConverter::term(cloner.m_allocator, new_amp);
            }

            NodeTerm* operator()(const NodeTermDrvalue* term_drval) const {
                NodeTermDrvalue* new_drval = cloner.m_allocator->emplace<NodeTermDrvalue>();
                new_drval->def = term_drval->def;
                new_drval->expr = cloner.clone_expr(term_drval->expr);
                return AstConverter::term(cloner.m_allocator, new_drval);
            }

            NodeTerm* operator()(const NodeTermIdent* term_ident) const {
            	return AstConverter::term(cloner.m_allocator, cloner.copy_simple(term_ident));
            }

            NodeTerm* operator()(const NodeTermNmIdent* term_ident) const {
                return AstConverter::term(cloner.m_allocator, cloner.copy_simple(term_ident));
            }

            NodeTerm* operator()(const NodeTermParen* term_paren) const {
                NodeTermParen* new_paren = cloner.m_allocator->emplace<NodeTermParen>();
                new_paren->expr = cloner.clone_expr(term_paren->expr);
                return AstConverter::term(cloner.m_allocator, new_paren);
            }

            NodeTerm* operator()(const NodeTermNmCall* term_call) const {
                NodeTermNmCall* new_nm_call = cloner.m_allocator->emplace<NodeTermNmCall>();
                new_nm_call->def = term_call->def;
                new_nm_call->name = term_call->name;
                new_nm_call->nm = term_call->nm;
                if(term_call->args.has_value()) new_nm_call->args = cloner.clone_expr(term_call->args.value());
                else new_nm_call->args = std::nullopt;
                new_nm_call->targs = term_call->targs;
                return AstConverter::term(cloner.m_allocator, new_nm_call);
            }

            NodeTerm* operator()(const NodeTermCall* term_call) const {
                NodeTermCall* new_call = cloner.m_allocator->emplace<NodeTermCall>();
                new_call->def = term_call->def;
                new_call->name = term_call->name;
                if(term_call->args.has_value()) new_call->args = cloner.clone_expr(term_call->args.value());
                else new_call->args = std::nullopt;
                new_call->targs = term_call->targs;
                new_call->as_expr = term_call->as_expr;
                return AstConverter::term(cloner.m_allocator, new_call);
            }

            NodeTerm* operator()(const NodeTermMtCall* term_call) const {
                NodeTermMtCall* new_mt_call = cloner.m_allocator->emplace<NodeTermMtCall>();
                new_mt_call->def = term_call->def;
                new_mt_call->mt = cloner.clone_expr(term_call->mt);
                new_mt_call->name = term_call->name;
                if(term_call->args.has_value()) new_mt_call->args = cloner.clone_expr(term_call->args.value());
                else new_mt_call->args = std::nullopt;
                new_mt_call->targs = term_call->targs;
                return AstConverter::term(cloner.m_allocator, new_mt_call);
            }
        };

        TermVisitor visitor{ *this };
        return std::visit(visitor, term->var);
    }

    template <typename T>
    NodeBinExpr* default_clone_bin_expr(T some_bin_expr, const NodeBinExpr* base) {
        remove_all_const_t<T> new_bin_expr = m_allocator->emplace<std::remove_pointer_t<remove_all_const_t<T>>>();
        new_bin_expr->lhs = clone_expr(some_bin_expr->lhs);
        new_bin_expr->rhs = clone_expr(some_bin_expr->rhs);
        return AstConverter::bin_expr(m_allocator, new_bin_expr, base->def);
    }

    NodeBinExpr* clone_bin_expr(const NodeBinExpr* bin_expr)
    {
        if(bin_expr == nullptr) {
            return nullptr;
        }
        struct BinExprVisitor {
            AstCloner& cloner;
            const NodeBinExpr* base;

            NodeBinExpr* operator()(const NodeBinExprSub* sub) const {
                return cloner.default_clone_bin_expr(sub, base);
            }

            NodeBinExpr* operator()(const NodeBinExprAdd* add) const {
                return cloner.default_clone_bin_expr(add, base);
            }

            NodeBinExpr* operator()(const NodeBinExprMulti* multi) const {
                return cloner.default_clone_bin_expr(multi, base);
            }

            NodeBinExpr* operator()(const NodeBinExprDiv* div) const {
                return cloner.default_clone_bin_expr(div, base);
            }

            NodeBinExpr* operator()(const NodeBinExprShl* shl) const {
                return cloner.default_clone_bin_expr(shl, base);
            }

            NodeBinExpr* operator()(const NodeBinExprShr* shr) const {
                return cloner.default_clone_bin_expr(shr, base);
            }

            NodeBinExpr* operator()(const NodeBinExprMod* md) const {
                return cloner.default_clone_bin_expr(md, base);
            }

            NodeBinExpr* operator()(const NodeBinExprEqEq* eqeq) const {
                return cloner.default_clone_bin_expr(eqeq, base);
            }

            NodeBinExpr* operator()(const NodeBinExprNotEq* nq) const {
                return cloner.default_clone_bin_expr(nq, base);
            }

            NodeBinExpr* operator()(const NodeBinExprLess* less) const {
                return cloner.default_clone_bin_expr(less, base);
            }

            NodeBinExpr* operator()(const NodeBinExprAnd* band) const {
                return cloner.default_clone_bin_expr(band, base);
            }

            NodeBinExpr* operator()(const NodeBinExprOr* bor) const {
                return cloner.default_clone_bin_expr(bor, base);
            }

            NodeBinExpr* operator()(const NodeBinExprAbove* above) const {
                return cloner.default_clone_bin_expr(above, base);
            }

            NodeBinExpr* operator()(const NodeBinExprDot* dot) const {
                return cloner.default_clone_bin_expr(dot, base);
            }

            NodeBinExpr* operator()(const NodeBinExprArgs* args) const {
                NodeBinExprArgs* new_args = cloner.m_allocator->emplace<NodeBinExprArgs>();
                GVector<NodeExpr*> n_args {};
                for(NodeExpr* arg : args->args) {
                    n_args.push_back(cloner.clone_expr(arg));
                }
                new_args->args = n_args;
                return AstConverter::bin_expr(cloner.m_allocator, new_args, base->def);
            }
            NodeBinExpr* operator()(const NodeBinExprIndex* idx) const {
                return cloner.default_clone_bin_expr(idx, base);
            }
        };

        BinExprVisitor visitor{ *this , bin_expr };
        return std::visit(visitor, bin_expr->var);
    }

    NodeExpr* clone_expr(const NodeExpr* expr)
    {
        if(expr == nullptr) {
            return nullptr;
        }
        struct ExprVisitor {
            AstCloner& cloner;

            NodeExpr* operator()(const NodeTerm* term) const
            {
                return AstConverter::expr(cloner.m_allocator, cloner.clone_term(term));
            }

            NodeExpr* operator()(const NodeBinExpr* bin_expr) const
            {
                return AstConverter::expr(cloner.m_allocator, cloner.clone_bin_expr(bin_expr));
            }
        };

        ExprVisitor visitor{ *this };
        return std::visit(visitor, expr->var);
    }

    NodeScope* clone_scope(const NodeScope* scope)
    {
        if(scope == nullptr) {
            return nullptr;
        }
        NodeScope* new_scope = m_allocator->emplace<NodeScope>();
        GVector<NodeStmt*> stmts;

        for(const NodeStmt* stmt : scope->stmts) {
            stmts.push_back(clone_stmt(stmt));
        }

        new_scope->stmts = stmts;
        return new_scope;
    }

    NodeIfPred* clone_if_pred(const NodeIfPred* pred)
    {
        if(pred == nullptr) {
            return nullptr;
        }
        struct PredVisitor {
            AstCloner& cloner;

            NodeIfPred* operator()(const NodeIfPredElif* elif) const
            {
                NodeIfPredElif* new_elif = cloner.m_allocator->emplace<NodeIfPredElif>();
                new_elif->expr = cloner.clone_expr(elif->expr);
                new_elif->scope = cloner.clone_scope(elif->scope);
                if(elif->pred.has_value()) new_elif->pred = cloner.clone_if_pred(elif->pred.value());
                else new_elif->pred = std::nullopt;
                return AstConverter::if_pred(cloner.m_allocator, new_elif);
            }

            NodeIfPred* operator()(const NodeIfPredElse* else_) const
            {
                NodeIfPredElse* new_else = cloner.m_allocator->emplace<NodeIfPredElse>();
                new_else->scope = cloner.clone_scope(else_->scope);
                return AstConverter::if_pred(cloner.m_allocator, new_else);
            }
        };

        PredVisitor visitor{ *this };
        return std::visit(visitor, pred->var);
    }

    NodeStmt* clone_stmt(const NodeStmt* stmt)
    {
        if(stmt == nullptr) {
            return nullptr;
        }
        struct StmtVisitor {
            AstCloner& cloner;

            NodeStmt* operator()([[maybe_unused]] const NodeStmtExit* stmt_exit) const
            {
                
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtProc* stmt_proc)
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtReturn* stmt_return) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtLet* stmt_let) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtCompileTimeIf* stmt_ctif) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtAssign* stmt_assign) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtIncBy* stmt_assign) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtDecBy* stmt_assign) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtMulBy* stmt_assign) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtDivBy* stmt_assign) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtCall* stmt_call) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeScope* scope) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtPushOnStack* stmt_push) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtIf* stmt_if) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtWhile* stmt_while) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtBreak* stmt_break) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtStore* stmt_store) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtBuffer* stmt_buf) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtAsm* stmt_asm) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtCextern* stmt_cextern) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtStruct* stmt_struct) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtInterface* stmt_inter) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtOninit* stmt_oninit) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtStaticAssert* stmt_st) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtDelete* stmt_delete) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtRaise* stmt_raise) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtNamespace* stmt_space) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtImpl* stmt_impl) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtNmCall* stmt_call) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtMtCall* stmt_call) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtConst* stmt_const) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtTypedef* stmt_tdef) const
            {
                return nullptr;
            }

            NodeStmt* operator()([[maybe_unused]] const NodeStmtTry* stmt_try) const
            {
                return nullptr; 
            }
            NodeStmt* operator()([[maybe_unused]] const NodeStmtFor* stmt_for) const
            {
                return nullptr;
            }
            NodeStmt* operator()([[maybe_unused]] const NodeStmtForeach* stmt_foreach) const
            {
                return nullptr;
            }
            NodeStmt* operator()([[maybe_unused]] const NodeStmtEnum* stmt_enum) const
            {
                return nullptr;
            }
        };

        StmtVisitor visitor{ *this };
        return std::visit(visitor, stmt->var);
    }
};