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
        struct TermVisitor {
            AstCloner& cloner;

            NodeTerm* operator()(const NodeTermIntLit* term_int_lit) const {
            	return AstConverter::term(cloner.m_allocator, cloner.copy_simple(term_int_lit));
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermType* tp) {
            	return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermCol* term_col) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermLine* term_line) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermPop* term_pop) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermExprStmt* term_stmt) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermFile* term_file) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermCtEval* term_eval) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermCtMdefined* term_mdef) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermSizeof* term_sizeof) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermRd* term_rd) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermCast* term_cast) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermUnref* term_unref) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermCastTo* term_cast_to) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermTypeid* term_typeid) const {
                return nullptr;
            }

            NodeTerm* operator()(const NodeTermStrLit* term_str_lit) const {
            	return AstConverter::term(cloner.m_allocator, cloner.copy_simple(term_str_lit));
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermAmpersand* term_amp) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermDrvalue* term_drval) const {
                return nullptr;
            }

            NodeTerm* operator()(const NodeTermIdent* term_ident) const {
            	return AstConverter::term(cloner.m_allocator, cloner.copy_simple(term_ident));
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermNmIdent* term_ident) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermParen* term_paren) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermNmCall* term_call) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermCall* term_call) const {
                return nullptr;
            }

            NodeTerm* operator()([[maybe_unused]] const NodeTermMtCall* term_call) const {
                return nullptr;
            }
        };

        TermVisitor visitor{ *this };
        return std::visit(visitor, term->var);
    }

    NodeBinExpr* clone_bin_expr(const NodeBinExpr* bin_expr)
    {
        struct BinExprVisitor {
            AstCloner& cloner;
            const NodeBinExpr* base;

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprSub* sub) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprAdd* add) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprMulti* multi) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprDiv* div) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprShl* shl) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprShr* shr) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprMod* md) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprEqEq* eqeq) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprNotEq* nq) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprLess* less) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprAnd* band) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprOr* bor) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprAbove* above) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprDot* dot) const {
                return nullptr;
            }

            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprArgs* args) const {
                return nullptr;
            }
            NodeBinExpr* operator()([[maybe_unused]] const NodeBinExprIndex* idx) const {
                return nullptr;
            }
        };

        BinExprVisitor visitor{ *this , bin_expr };
        return std::visit(visitor, bin_expr->var);
    }

    NodeExpr* clone_expr(const NodeExpr* expr)
    {
        struct ExprVisitor {
            AstCloner& cloner;

            NodeExpr* operator()([[maybe_unused]] const NodeTerm* term) const
            {
                return nullptr;
            }

            NodeExpr* operator()([[maybe_unused]] const NodeBinExpr* bin_expr) const
            {
                return nullptr;
            }
        };

        ExprVisitor visitor{ *this };
        return std::visit(visitor, expr->var);
    }

    NodeScope* gen_scope([[maybe_unused]] const NodeScope* scope)
    {
        return nullptr;
    }

    NodeIfPred* clone_if_pred(const NodeIfPred* pred)
    {
        struct PredVisitor {
            AstCloner& cloner;

            NodeIfPred* operator()([[maybe_unused]] const NodeIfPredElif* elif) const
            {
                return nullptr;
            }

            NodeIfPred* operator()([[maybe_unused]] const NodeIfPredElse* else_) const
            {
                return nullptr;
            }
        };

        PredVisitor visitor{ *this };
        return std::visit(visitor, pred->var);
    }

    NodeStmt* clone_stmt(const NodeStmt* stmt)
    {
        struct StmtVisitor {
            AstCloner& cloner;

            NodeStmt* operator()([[maybe_unused]] const NodeStmtExit* stmt_exit) const
            {
                return nullptr;
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