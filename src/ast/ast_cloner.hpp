#pragma once

class AstCloner {
public:

    explicit AstCloner(ArenaAllocator* arena) {
        m_allocator = arena;
    }

    template <typename T>
    T* copy_simple(T* object) {
    	return m_allocator->emplace<T>(*object);
    }

    NodeTerm* clone_term(const NodeTerm* term)
    {
        struct TermVisitor {
            AstCloner& cloner;

            NodeTerm* operator()(const NodeTermIntLit* term_int_lit) const {
            	return cloner.copy_simple(term_int_lit);
            }

            NodeTerm* operator()(const NodeTermType* tp) {

            }

            NodeTerm* operator()(const NodeTermCol* term_col) const {

            }

            NodeTerm* operator()(const NodeTermLine* term_line) const {

            }

            NodeTerm* operator()(const NodeTermPop* term_pop) const {
 
            }

            NodeTerm* operator()(const NodeTermExprStmt* term_stmt) const {

            }

            NodeTerm* operator()(const NodeTermFile* term_file) const {

            }

            NodeTerm* operator()(const NodeTermCtEval* term_eval) const {

            }

            NodeTerm* operator()(const NodeTermCtMdefined* term_mdef) const {

            }

            NodeTerm* operator()(NodeTermSizeof* term_sizeof) const {

            }

            NodeTerm* operator()(const NodeTermRd* term_rd) const {

            }

            NodeTerm* operator()(const NodeTermCast* term_cast) const {

            }

            NodeTerm* operator()(const NodeTermUnref* term_unref) const {
                
            }

            NodeTerm* operator()(const NodeTermCastTo* term_cast_to) const {

            }

            NodeTerm* operator()(NodeTermTypeid* term_typeid) const {
                
            }

            NodeTerm* operator()(const NodeTermStrLit* term_str_lit) const {
            	return cloner.copy_simple(term_str_lit);
            }

            NodeTerm* operator()(const NodeTermAmpersand* term_amp) const {

            }

            NodeTerm* operator()(const NodeTermDrvalue* term_drval) const {

            }

            NodeTerm* operator()(const NodeTermIdent* term_ident) const {
            	return cloner.copy_simple(term_ident);
            }

            NodeTerm* operator()(const NodeTermNmIdent* term_ident) const {
    
            }

            NodeTerm* operator()(const NodeTermParen* term_paren) const {

            }

            NodeTerm* operator()(NodeTermNmCall* term_call) const {

            }

            NodeTerm* operator()(NodeTermCall* term_call) const {

            }

            NodeTerm* operator()(const NodeTermMtCall* term_call) const {
               
            }
        };

        TermVisitor visitor{ *this };
        return std::visit(visitor, term->var);
    }

    NodeBinExpr* clone_bin_expr(const NodeBinExpr* bin_expr)
    {
        struct BinExprVisitor {
            AstCloner& cloner;

            NodeBinExpr* operator()(const NodeBinExprSub* sub) const {
 
            }

            NodeBinExpr* operator()(const NodeBinExprAdd* add) const {

            }

            NodeBinExpr* operator()(const NodeBinExprMulti* multi) const {

            }

            NodeBinExpr* operator()(const NodeBinExprDiv* div) const {

            }

            NodeBinExpr* operator()(const NodeBinExprShl* shl) const {
              
            }

            NodeBinExpr* operator()(const NodeBinExprShr* shr) const {
                
            }

            NodeBinExpr* operator()(const NodeBinExprMod* md) const {
                
            }

            NodeBinExpr* operator()(const NodeBinExprEqEq* eqeq) const {
               
            }

            NodeBinExpr* operator()(const NodeBinExprNotEq* nq) const {
              
            }

            NodeBinExpr* operator()(const NodeBinExprLess* less) const {
              
            }

            NodeBinExpr* operator()(const NodeBinExprAnd* band) const {
   
            }

            NodeBinExpr* operator()(const NodeBinExprOr* bor) const {

            }

            NodeBinExpr* operator()(const NodeBinExprAbove* above) const {

            }

            NodeBinExpr* operator()(const NodeBinExprDot* dot) const {
                
            }

            NodeBinExpr* operator()(const NodeBinExprArgs* args) const {

            }
            NodeBinExpr* operator()(const NodeBinExprIndex* idx) const {
                
            }
        };

        BinExprVisitor visitor{ *this };
        return std::visit(visitor, bin_expr->var);
    }

    NodeExpr* clone_expr(const NodeExpr* expr)
    {
        struct ExprVisitor {
            AstCloner& cloner;

            NodeExpr* operator()(const NodeTerm* term) const
            {
    
            }

            NodeExpr* operator()(const NodeBinExpr* bin_expr) const
            {

            }
        };

        ExprVisitor visitor{ *this, lvalue };
        return std::visit(visitor, expr->var);
    }

    NodeScope* gen_scope(const NodeScope* scope)
    {
        for (const NodeStmt* stmt : scope->stmts) {
            clone_stmt(stmt);
        }
    }

    NodeIfPred* clone_if_pred(const NodeIfPred* pred)
    {
        struct PredVisitor {
            AstCloner& cloner;

            NodeIfPred* operator()(const NodeIfPredElif* elif) const
            {

            }

            NodeIfPred* operator()(const NodeIfPredElse* else_) const
            {

            }
        };

        PredVisitor visitor{ *this };
        return std::visit(visitor, pred->var);
    }

    NodeStmt* clone_stmt(const NodeStmt* stmt)
    {
        struct StmtVisitor {
            AstCloner& cloner;

            NodeStmt* operator()(const NodeStmtExit* stmt_exit) const
            {

            }

            NodeStmt* operator()(const NodeStmtProc* stmt_proc)
            {

            }

            NodeStmt* operator()(const NodeStmtReturn* stmt_return) const
            {

            }

            NodeStmt* operator()(const NodeStmtLet* stmt_let) const
            {

            }

            NodeStmt* operator()(const NodeStmtCompileTimeIf* stmt_ctif) const
            {
                
            }

            NodeStmt* operator()(const NodeStmtAssign* stmt_assign) const
            {
                
            }

            NodeStmt* operator()(const NodeStmtIncBy* stmt_assign) const
            {
                
            }

            NodeStmt* operator()(const NodeStmtDecBy* stmt_assign) const
            {

            }

            NodeStmt* operator()(const NodeStmtMulBy* stmt_assign) const
            {
                
            }

            NodeStmt* operator()(const NodeStmtDivBy* stmt_assign) const
            {
               
            }

            NodeStmt* operator()(NodeStmtCall* stmt_call) const
            {

            }

            NodeStmt* operator()(const NodeScope* scope) const
            {
     
            }

            NodeStmt* operator()(const NodeStmtPushOnStack* stmt_push) const
            {
         
            }

            NodeStmt* operator()(const NodeStmtIf* stmt_if) const
            {
                
            }

            NodeStmt* operator()(const NodeStmtWhile* stmt_while) const
            {
                
            }

            NodeStmt* operator()(const NodeStmtBreak* stmt_break) const
            {
               
            }

            NodeStmt* operator()(const NodeStmtStore* stmt_store) const
            {
                
            }

            NodeStmt* operator()(const NodeStmtBuffer* stmt_buf) const
            {
               
            }

            NodeStmt* operator()(const NodeStmtAsm* stmt_asm) const
            {
               
            }

            NodeStmt* operator()(const NodeStmtCextern* stmt_cextern) const
            {
              
            }

            NodeStmt* operator()(const NodeStmtStruct* stmt_struct) const
            {
               
            }

            NodeStmt* operator()(const NodeStmtInterface* stmt_inter) const
            {
                
            }

            NodeStmt* operator()(const NodeStmtOninit* stmt_oninit) const
            {
               
            }

            NodeStmt* operator()(const NodeStmtStaticAssert* stmt_st) const
            {
               
            }

            NodeStmt* operator()(const NodeStmtDelete* stmt_delete) const
            {
                
            }

            NodeStmt* operator()(const NodeStmtRaise* stmt_raise) const
            {
               
            }

            NodeStmt* operator()(const NodeStmtNamespace* stmt_space) const
            {

            }

            NodeStmt* operator()(NodeStmtImpl* stmt_impl) const
            {
                
            }

            NodeStmt* operator()(NodeStmtNmCall* stmt_call) const
            {
               
            }

            NodeStmt* operator()(const NodeStmtMtCall* stmt_call) const
            {
               
            }

            NodeStmt* operator()(const NodeStmtConst* stmt_const) const
            {
               
            }

            NodeStmt* operator()(const NodeStmtTypedef* stmt_tdef) const
            {
              
            }

            NodeStmt* operator()(const NodeStmtTry* stmt_try) const
            {
                
            }
            NodeStmt* operator()(const NodeStmtFor* stmt_for) const
            {
                
            }
            NodeStmt* operator()(const NodeStmtForeach* stmt_foreach) const
            {
            
            }
            NodeStmt* operator()(const NodeStmtEnum* stmt_enum) const
            {
   
            }
        };

        StmtVisitor visitor{ *this };
        return std::visit(visitor, stmt->var);
    }
};