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

            NodeStmt* operator()(const NodeStmtExit* stmt_exit) const
            {
                NodeStmtExit* new_exit = cloner.m_allocator->emplace<NodeStmtExit>();
                new_exit->def = stmt_exit->def;
                new_exit->expr = cloner.clone_expr(stmt_exit->expr);
                return AstConverter::stmt(cloner.m_allocator, stmt_exit);
            }

            NodeStmt* operator()(const NodeStmtProc* stmt_proc)
            {
                NodeStmtProc* new_proc = cloner.m_allocator->emplace<NodeStmtProc>();
                new_proc->name = stmt_proc->name;
                new_proc->def = stmt_proc->def;
                new_proc->rettype = stmt_proc->rettype;
                new_proc->attrs = stmt_proc->attrs;
                new_proc->params = stmt_proc->params;
                new_proc->scope = cloner.clone_scope(stmt_proc->scope);
                new_proc->prototype = stmt_proc->prototype;
                new_proc->templates = NULL;
                new_proc->decorators = stmt_proc->decorators;
                if(stmt_proc->templates != NULL) {
                    GVector<GString>* new_temps = cloner.m_allocator->emplace<GVector<GString>>();
                    for(size_t i = 0;i < stmt_proc->templates->size();i++) {
                        new_temps->push_back(stmt_proc->templates->operator[](i));
                    }
                    new_proc->templates = new_temps;
                }
                new_proc->constraints = stmt_proc->constraints;
                return AstConverter::stmt(cloner.m_allocator, new_proc);
            }

            NodeStmt* operator()(const NodeStmtReturn* stmt_return) const
            {
                NodeStmtReturn* new_return = cloner.m_allocator->emplace<NodeStmtReturn>();
                new_return->def = stmt_return->def;
                new_return->expr = std::nullopt;
                if(stmt_return->expr.has_value()) new_return->expr = cloner.clone_expr(stmt_return->expr.value());
                return AstConverter::stmt(cloner.m_allocator, new_return);
            }

            NodeStmt* operator()(const NodeStmtLet* stmt_let) const
            {
                NodeStmtLet* new_let = cloner.m_allocator->emplace<NodeStmtLet>();
                new_let->ident = stmt_let->ident;
                new_let->expr = std::nullopt;
                new_let->type = std::nullopt;
                if(stmt_let->expr.has_value()) new_let->expr = cloner.clone_expr(stmt_let->expr.value());
                if(stmt_let->type.has_value()) new_let->type = stmt_let->type.value();
                return AstConverter::stmt(cloner.m_allocator, new_let);
            }

            NodeStmt* operator()(const NodeStmtCompileTimeIf* stmt_ctif) const
            {
                NodeStmtCompileTimeIf* new_ctif = cloner.m_allocator->emplace<NodeStmtCompileTimeIf>();
                new_ctif->def = stmt_ctif->def;
                new_ctif->condition = cloner.clone_expr(stmt_ctif->condition);
                new_ctif->_if = cloner.clone_scope(stmt_ctif->_if);
                new_ctif->_else = std::nullopt;
                if(stmt_ctif->_else.has_value()) new_ctif->_else = cloner.clone_scope(stmt_ctif->_else.value());
                return AstConverter::stmt(cloner.m_allocator, new_ctif);
            }

            NodeStmt* operator()(const NodeStmtAssign* stmt_assign) const
            {
                NodeStmtAssign* new_assign = cloner.m_allocator->emplace<NodeStmtAssign>();
                new_assign->expr = cloner.clone_expr(stmt_assign->expr);
                new_assign->lvalue = cloner.clone_expr(stmt_assign->lvalue);
                new_assign->def = stmt_assign->def;
                return AstConverter::stmt(cloner.m_allocator, new_assign);
            }

            NodeStmt* operator()(const NodeStmtIncBy* stmt_assign) const
            {
                NodeStmtIncBy* new_inc = cloner.m_allocator->emplace<NodeStmtIncBy>();
                new_inc->def = stmt_assign->def;
                new_inc->lvalue = cloner.clone_expr(stmt_assign->lvalue);
                new_inc->expr = cloner.clone_expr(stmt_assign->expr);
                return AstConverter::stmt(cloner.m_allocator, new_inc);
            }

            NodeStmt* operator()(const NodeStmtDecBy* stmt_assign) const
            {
                NodeStmtDecBy* new_dec = cloner.m_allocator->emplace<NodeStmtDecBy>();
                new_dec->def = stmt_assign->def;
                new_dec->lvalue = cloner.clone_expr(stmt_assign->lvalue);
                new_dec->expr = cloner.clone_expr(stmt_assign->expr);
                return AstConverter::stmt(cloner.m_allocator, new_dec);
            }

            NodeStmt* operator()(const NodeStmtMulBy* stmt_assign) const
            {
                NodeStmtMulBy* new_mul = cloner.m_allocator->emplace<NodeStmtMulBy>();
                new_mul->def = stmt_assign->def;
                new_mul->lvalue = cloner.clone_expr(stmt_assign->lvalue);
                new_mul->expr = cloner.clone_expr(stmt_assign->expr);
                return AstConverter::stmt(cloner.m_allocator, new_mul);
            }

            NodeStmt* operator()(const NodeStmtDivBy* stmt_assign) const
            {
                NodeStmtDivBy* new_div = cloner.m_allocator->emplace<NodeStmtDivBy>();
                new_div->def = stmt_assign->def;
                new_div->lvalue = cloner.clone_expr(stmt_assign->lvalue);
                new_div->expr = cloner.clone_expr(stmt_assign->expr);
                return AstConverter::stmt(cloner.m_allocator, new_div);
            }

            NodeStmt* operator()(const NodeStmtCall* stmt_call) const
            {
                NodeStmtCall* new_call = cloner.m_allocator->emplace<NodeStmtCall>();
                new_call->def = stmt_call->def;
                new_call->name = stmt_call->name;
                new_call->args = std::nullopt;
                if(stmt_call->args.has_value()) new_call->args = cloner.clone_expr(stmt_call->args.value());
                new_call->targs = stmt_call->targs;
                new_call->resolved_expression = nullptr;
                if(stmt_call->resolved_expression != nullptr) new_call->resolved_expression = cloner.clone_expr(stmt_call->resolved_expression);
                return AstConverter::stmt(cloner.m_allocator, new_call);
            }

            NodeStmt* operator()(const NodeScope* scope) const
            {
                return AstConverter::stmt(cloner.m_allocator, cloner.clone_scope(scope));
            }

            NodeStmt* operator()(const NodeStmtPushOnStack* stmt_push) const
            {
                NodeStmtPushOnStack* new_push = cloner.m_allocator->emplace<NodeStmtPushOnStack>();
                new_push->def = stmt_push->def;
                new_push->expr = cloner.clone_expr(stmt_push->expr);
                return AstConverter::stmt(cloner.m_allocator, new_push);
            }

            NodeStmt* operator()(const NodeStmtIf* stmt_if) const
            {
                NodeStmtIf* new_if = cloner.m_allocator->emplace<NodeStmtIf>();
                new_if->expr = cloner.clone_expr(stmt_if->expr);
                new_if->scope = cloner.clone_scope(stmt_if->scope);
                new_if->pred = std::nullopt;
                if(stmt_if->pred.has_value()) new_if->pred = cloner.clone_if_pred(stmt_if->pred.value());
                return AstConverter::stmt(cloner.m_allocator, new_if);
            }

            NodeStmt* operator()(const NodeStmtWhile* stmt_while) const
            {
                NodeStmtWhile* new_while = cloner.m_allocator->emplace<NodeStmtWhile>();
                new_while->expr = cloner.clone_expr(stmt_while->expr);
                new_while->scope = cloner.clone_scope(stmt_while->scope);
                return AstConverter::stmt(cloner.m_allocator, new_while);
            }

            NodeStmt* operator()(const NodeStmtBreak* stmt_break) const
            {
                return AstConverter::stmt(cloner.m_allocator, cloner.copy_simple(stmt_break));
            }

            NodeStmt* operator()(const NodeStmtStore* stmt_store) const
            {
                NodeStmtStore* new_store = cloner.m_allocator->emplace<NodeStmtStore>();
                new_store->def = stmt_store->def;
                new_store->size = stmt_store->size;
                new_store->ptr = cloner.clone_expr(stmt_store->ptr);
                new_store->expr = cloner.clone_expr(stmt_store->expr);
                return AstConverter::stmt(cloner.m_allocator, new_store);
            }

            NodeStmt* operator()(const NodeStmtBuffer* stmt_buf) const
            {
                NodeStmtBuffer* new_buffer = cloner.m_allocator->emplace<NodeStmtBuffer>();
                new_buffer->def = stmt_buf->def;
                new_buffer->name = stmt_buf->name;
                new_buffer->size = cloner.clone_expr(stmt_buf->size);
                return AstConverter::stmt(cloner.m_allocator, new_buffer);
            }

            NodeStmt* operator()(const NodeStmtAsm* stmt_asm) const
            {
                return AstConverter::stmt(cloner.m_allocator, cloner.copy_simple(stmt_asm));
            }

            NodeStmt* operator()(const NodeStmtCextern* stmt_cextern) const
            {
                return AstConverter::stmt(cloner.m_allocator, cloner.copy_simple(stmt_cextern));
            }

            NodeStmt* operator()(const NodeStmtStruct* stmt_struct) const
            {
                return AstConverter::stmt(cloner.m_allocator, cloner.copy_simple(stmt_struct));
            }

            NodeStmt* operator()(const NodeStmtInterface* stmt_inter) const
            {
                return AstConverter::stmt(cloner.m_allocator, cloner.copy_simple(stmt_inter));
            }

            NodeStmt* operator()(const NodeStmtOninit* stmt_oninit) const
            {
                NodeStmtOninit* new_oninit = cloner.m_allocator->emplace<NodeStmtOninit>();
                new_oninit->def = stmt_oninit->def;
                new_oninit->scope = cloner.clone_scope(stmt_oninit->scope);
                return AstConverter::stmt(cloner.m_allocator, new_oninit);
            }

            NodeStmt* operator()(const NodeStmtStaticAssert* stmt_st) const
            {
                NodeStmtStaticAssert* new_st = cloner.m_allocator->emplace<NodeStmtStaticAssert>();
                new_st->def = stmt_st->def;
                new_st->msg = stmt_st->msg;
                new_st->condition = cloner.clone_expr(stmt_st->condition);
                return AstConverter::stmt(cloner.m_allocator, new_st);
            }

            NodeStmt* operator()(const NodeStmtDelete* stmt_delete) const
            {
                NodeStmtDelete* new_delete = cloner.m_allocator->emplace<NodeStmtDelete>();
                new_delete->def = stmt_delete->def;
                new_delete->expr = cloner.clone_expr(stmt_delete->expr);
                return AstConverter::stmt(cloner.m_allocator, new_delete);
            }

            NodeStmt* operator()(const NodeStmtRaise* stmt_raise) const
            {
                NodeStmtRaise* new_raise = cloner.m_allocator->emplace<NodeStmtRaise>();
                new_raise->def = stmt_raise->def;
                new_raise->expr = cloner.clone_expr(stmt_raise->expr);
                return AstConverter::stmt(cloner.m_allocator, new_raise);
            }

            NodeStmt* operator()(const NodeStmtNamespace* stmt_space) const
            {
                NodeStmtNamespace* new_namespace = cloner.m_allocator->emplace<NodeStmtNamespace>();
                new_namespace->def = stmt_space->def;
                new_namespace->name = stmt_space->name;
                new_namespace->scope = cloner.clone_scope(stmt_space->scope);
                return AstConverter::stmt(cloner.m_allocator, new_namespace);
            }

            NodeStmt* operator()(const NodeStmtImpl* stmt_impl) const
            {
                NodeStmtImpl* new_impl = cloner.m_allocator->emplace<NodeStmtImpl>();
                new_impl->def = stmt_impl->def;
                new_impl->name = stmt_impl->name;
                new_impl->temps = stmt_impl->temps;
                new_impl->inst = stmt_impl->inst;
                new_impl->scope = cloner.clone_scope(stmt_impl->scope);
                return AstConverter::stmt(cloner.m_allocator, new_impl);
            }

            NodeStmt* operator()(const NodeStmtNmCall* stmt_call) const
            {
                NodeStmtNmCall* new_call = cloner.m_allocator->emplace<NodeStmtNmCall>();
                new_call->def = stmt_call->def;
                new_call->name = stmt_call->name;
                new_call->nm = stmt_call->nm;
                new_call->targs = stmt_call->targs;
                new_call->args = std::nullopt;
                if(stmt_call->args.has_value()) new_call->args = cloner.clone_expr(stmt_call->args.value());
                return AstConverter::stmt(cloner.m_allocator, new_call);
            }

            NodeStmt* operator()(const NodeStmtMtCall* stmt_call) const
            {
                NodeStmtMtCall* new_call = cloner.m_allocator->emplace<NodeStmtMtCall>();
                new_call->def = stmt_call->def;
                new_call->name = stmt_call->name;
                new_call->targs = stmt_call->targs;
                new_call->mt = cloner.clone_expr(stmt_call->mt);
                new_call->args = std::nullopt;
                if(stmt_call->args.has_value()) new_call->args = cloner.clone_expr(stmt_call->args.value());
                return AstConverter::stmt(cloner.m_allocator, new_call);
            }

            NodeStmt* operator()(const NodeStmtConst* stmt_const) const
            {
                NodeStmtConst* new_const = cloner.m_allocator->emplace<NodeStmtConst>();
                new_const->def = stmt_const->def;
                new_const->name = stmt_const->name;
                new_const->expr = cloner.clone_expr(stmt_const->expr);
                return AstConverter::stmt(cloner.m_allocator, new_const);
            }

            NodeStmt* operator()(const NodeStmtTypedef* stmt_tdef) const
            {
                return AstConverter::stmt(cloner.m_allocator, cloner.copy_simple(stmt_tdef));
            }

            NodeStmt* operator()(const NodeStmtTry* stmt_try) const
            {
                NodeStmtTry* new_try = cloner.m_allocator->emplace<NodeStmtTry>();
                new_try->def = stmt_try->def;
                new_try->name = stmt_try->name;
                new_try->type = stmt_try->type;
                new_try->_try = cloner.clone_scope(stmt_try->_try);
                new_try->_catch = cloner.clone_scope(stmt_try->_catch);
                return AstConverter::stmt(cloner.m_allocator, new_try);
            }
            NodeStmt* operator()(const NodeStmtFor* stmt_for) const
            {
                NodeStmtFor* new_for = cloner.m_allocator->emplace<NodeStmtFor>();
                new_for->init = cloner.clone_stmt(stmt_for->init);
                new_for->cond = cloner.clone_expr(stmt_for->cond);
                new_for->step = cloner.clone_stmt(stmt_for->step);
                new_for->scope = cloner.clone_scope(stmt_for->scope);
                return AstConverter::stmt(cloner.m_allocator, new_for);
            }
            NodeStmt* operator()(const NodeStmtForeach* stmt_foreach) const
            {
                NodeStmtForeach* new_foreach = cloner.m_allocator->emplace<NodeStmtForeach>();
                new_foreach->var_name = stmt_foreach->var_name;
                new_foreach->expr = cloner.clone_expr(stmt_foreach->expr);
                new_foreach->scope = cloner.clone_scope(stmt_foreach->scope);
                return AstConverter::stmt(cloner.m_allocator, new_foreach);
            }
            NodeStmt* operator()(const NodeStmtEnum* stmt_enum) const
            {
                return AstConverter::stmt(cloner.m_allocator, cloner.copy_simple(stmt_enum));
            }
        };

        StmtVisitor visitor{ *this };
        return std::visit(visitor, stmt->var);
    }

    NodeProg* clone_prog(NodeProg* prog) {
        NodeProg* new_prog = m_allocator->emplace<NodeProg>();
        for(NodeStmt* stmt : prog->stmts) {
            new_prog->stmts.push_back(stmt);
        }
        return new_prog;
    }
};