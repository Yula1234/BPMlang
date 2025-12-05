#pragma once

struct Procedure {
	std::string name;
	std::vector<std::pair<std::string, DataType>> params;
	DataType rettype;
	size_t stack_allign;
	std::vector<ProcAttr> attrs;
	Token def;
	bool prototype;
	bool overload;
	std::vector<std::string>* templates;
    NodeStmtProc* from;
    std::string mangled_symbol;
    std::vector<Procedure*> overloads;
    std::string return_label;
};

struct Variable {
    std::string name;
    DataType type;
    size_t stack_loc;
};

struct GlobalVariable {
    std::string name;
    DataType type;
    std::string mangled_symbol;
};

struct Struct {
    std::string name;
    std::unordered_map<std::string, std::pair<DataType, int>> fields; // name => pair<Type, Position in Order>
    size_t _typeid;
    std::vector<std::string> temps;
    Token def;
    std::optional<DataType> parent_type;
};

namespace std {
template<>
struct hash<DataType> {
    size_t operator()(const DataType& p) const noexcept {
        std::hash<std::string> h;
        return h(p.to_string());
    }
};
}

class SemanticScope;

class Namespace {
public:
	std::string name;
	SemanticScope* scope = nullptr;
	Namespace* parent;
	Namespace(const std::string& name, SemanticScope* scope, Namespace* parent) : name(name), scope(scope), parent(parent) {}
};



class SemanticScope {
public:
    std::unordered_map<std::string, Procedure*> m_procs;
    std::unordered_map<std::string, Struct*> m_structs;
    std::unordered_map<std::string, Variable*> m_vars;
    std::unordered_map<std::string, Namespace*> m_namespaces;
    std::unordered_map<std::string, GlobalVariable*> m_gvars;

    std::optional<Procedure*> proc_lookup(const std::string& name) const {
        const auto& search = m_procs.find(name);
        if(search != m_procs.end()) return search->second;
        return std::nullopt;
    }
    std::optional<Variable*> var_lookup(const std::string& name) const {
        const auto& search = m_vars.find(name);
        if(search != m_vars.end()) return search->second;
        return std::nullopt;
    }
    std::optional<Namespace*> namespace_lookup(const std::string& name) const {
        const auto& search = m_namespaces.find(name);
        if(search != m_namespaces.end()) return search->second;
        return std::nullopt;
    }
    std::optional<Struct*> struct_lookup(const std::string& name) const {
        const auto& search = m_structs.find(name);
        if(search != m_structs.end()) return search->second;
        return std::nullopt;
    }
    std::optional<GlobalVariable*> gvar_lookup(const std::string& name) const {
        const auto& search = m_gvars.find(name);
        if(search != m_gvars.end()) return search->second;
        return std::nullopt;
    }
};


#define TYPEID_INT  0
#define TYPEID_PTR  1
#define TYPEID_VOID 2
#define TYPEID_ANY  3
#define TYPEID_CHAR 4
#define TYPEID_PROCPTR 5


class SemanticSymbolTable {
public:
    std::unordered_map<std::string, GlobalVariable*> m_gvars;
    std::deque<SemanticScope> m_scopes;

	SemanticSymbolTable() = default;

    size_t typeid_of(const Token& def, const DataType& type) noexcept {
        check_type_is_valid(def, type);
        if (!type.root().is_object) {
            SimpleDataType stype = type.root().getsimpletype();
            switch (stype) {
            case SimpleDataType::_int:       return TYPEID_INT;
            case SimpleDataType::_void:      return TYPEID_VOID;
            case SimpleDataType::ptr:        return TYPEID_PTR;
            case SimpleDataType::any:        return TYPEID_ANY;
            case SimpleDataType::_char:      return TYPEID_CHAR;
            case SimpleDataType::_constexpr: return 0ULL;
            case SimpleDataType::proc_ptr:   return TYPEID_PROCPTR;
            }
        } else {
            std::string sname = type.root().getobjectname();
            std::optional<Struct> st = m_sym_table.struct_lookup(sname);
            if (st.has_value())  return st.value().m_typeid;
        }
        assert(false);
        return 0;
    }

    std::optional<Procedure*> proc_lookup(const std::string& name) {
        for(const SemanticScope& scope : m_scopes) {
            std::optional<Procedure*> res = scope.proc_lookup(name);
            if(res.has_value()) return res;
        }
        return std::nullopt;
    }
    std::optional<Variable*> var_lookup(const std::string& name) {
        for(const SemanticScope& scope : m_scopes) {
            std::optional<Variable*> res = scope.var_lookup(name);
            if(res.has_value()) return res;
        }
        return std::nullopt;
    }
    std::optional<Namespace*> namespace_lookup(const std::string& name) {
        for(const SemanticScope& scope : m_scopes) {
            std::optional<Namespace*> res = scope.namespace_lookup(name);
            if(res.has_value()) return res;
        }
        return std::nullopt;
    }
    std::optional<Struct*> struct_lookup(const std::string& name) {
        for(const SemanticScope& scope : m_scopes) {
            std::optional<Struct*> res = scope.struct_lookup(name);
            if(res.has_value()) return res;
        }
        return std::nullopt;
    }
    std::optional<GlobalVariable*> gvar_lookup(const std::string& name) {
        const auto& search = m_gvars.find(name);
        if(search != m_gvars.end()) return search->second;
        return std::nullopt;
    }
    inline SemanticScope* last_scope() {
        assert(!m_scopes.empty());
        return &(m_scopes[0]);
    }
    void begin_scope() {
        m_scopes.push_front({});
    }
    void end_scope() {
        m_scopes.pop_front();
    }
};




class SemanticContext {
public:

    SemanticSymbolTable m_sym_table;

    Namespace* m_cur_namespace = nullptr;
    Procedure* m_cur_procedure = nullptr;

    DiagnosticManager* m_diag_man;
    ArenaAllocator* m_allocator;

	explicit SemanticContext(NodeProg* prog, DiagnosticManager* dman, ArenaAllocator* arena) {
		m_diag_man = dman;
		m_prog = prog;
		m_allocator = arena;
	}

    template <typename T>
    NodeExpr* as_expr_pointer(T term_or_bin_expr) {
        NodeExpr* as_expr = m_allocator->emplace<NodeExpr>();
        as_expr->var = term_or_bin_expr;
        return as_expr;
    }

    NodeExpr* construct_args_from_vector(const Token& def, const std::vector<NodeExpr*>& args) {
        NodeBinExprArgs* bin_expr_args = m_allocator->emplace<NodeBinExprArgs>();
        bin_expr_args->args = args;
        NodeBinExpr* bin_expr = m_allocator->emplace<NodeBinExpr>();
        bin_expr->var = bin_expr_args;
        bin_expr->def = def;
        return as_expr_pointer(bin_expr);
    }

    NodeExpr* construct_method_call(const Token& def, NodeExpr* object, const std::string& name,
                                    const std::vector<NodeExpr*>& args, const std::vector<DataType>& targs)
    {
        NodeTermMtCall* method_call = m_allocator->emplace<NodeTermMtCall>();
        method_call->def = def;
        method_call->mt = object;
        method_call->name = name;
        if(!args.empty()) { method_call->args = construct_args_from_vector(def, args); }
        else { method_call->args = std::nullopt; }
        method_call->targs = targs;
        NodeTerm* as_term = m_allocator->emplace<NodeTerm>();
        as_term->var = method_call;
        return as_expr_pointer(as_term);
    }

    void check_type_is_valid(const Token& def, const DataType& type) {
        if(type.root().is_simple()) return;

        for(const DataType& gn : type.node->generics) {
            check_type_is_valid(def, gn);
        }

        const std::string& object_name = type.getobjectname();

        const auto& search = m_validated_types.find(object_name);

        if(search != m_validated_types.end()) return;

        if(!m_sym_table.struct_lookup(object_name).has_value()) {
            m_diag_man->DiagnosticMessage(def, "error", "undefined type `" + object_name + "`", 0);
            exit(EXIT_FAILURE);
        }

        m_validated_types[object_name] = true;
    }

    enum class DefBinaryOpKind {
        ADD,
        SUB,
        MUL,
        DIV,
        SHL,
        SHR,
        MOD,
        EQUAL,
        NOT_EQUAL,
        LESS,
        LOGIC_AND,
        LOGIC_OR,
        ABOVE,
    };

    template <typename T>
    DataType analyze_default_bin_expr(T* bin_expr, DefBinaryOpKind kind, const NodeBinExpr* base, NodeExpr* base_expr, std::optional<DataType> ret_if_simple = std::nullopt)
    {
        const DataType& lhs_type = analyze_expr(bin_expr->lhs);
        const DataType& rhs_type = analyze_expr(bin_expr->rhs);
        if(lhs_type.is_object()) {
            std::vector<NodeExpr*> prepared_args({bin_expr->lhs, bin_expr->rhs});
            NodeExpr* method_call = construct_method_call(base->def, bin_expr->lhs, m_method_name_by_binop_kind[kind], prepared_args, {});
            base_expr->var = method_call->var;
            return analyze_expr(method_call);
        }
        if(!types_equ(lhs_type, rhs_type)) {
            char tmp_buffer[256];

            const std::pair<const char*, bool>& err_message = m_error_message_by_binop_kind[kind];
            if(err_message.second) {
                sprintf(tmp_buffer, err_message.first, rhs_type.to_string().c_str(), lhs_type.to_string().c_str());
            } else {
                sprintf(tmp_buffer, err_message.first, lhs_type.to_string().c_str(), rhs_type.to_string().c_str());
            }

            m_diag_man->DiagnosticMessage(base->def, "error", tmp_buffer, 0);
            exit(EXIT_FAILURE);
        }
        if(ret_if_simple.has_value()) return ret_if_simple.value();
        return lhs_type;
    }

    DataType analyze_term(const NodeTerm* term, NodeExpr* base_expr, bool lvalue = false)
    {
        struct TermVisitor {
            SemanticContext& sema;
            NodeExpr* base_expr;
            bool       lvalue;

            DataType operator()(const NodeTermIntLit* term_int_lit) const {
                return BaseDataTypeInt;
            }

            DataType operator()(const NodeTermType* tp) {
                sema.m_diag_man->DiagnosticMessage(tp->def, "error", "the ct_type() expression can only be used in compile-time calculations.", 0);
                exit(EXIT_FAILURE);
            }

            DataType operator()(const NodeTermCol* term_col) const {

            }

            DataType operator()(const NodeTermLine* term_line) const {

            }

            DataType operator()(const NodeTermPop* term_pop) const {

            }

            DataType operator()(const NodeTermExprStmt* term_stmt) const {
 
            }

            DataType operator()(const NodeTermFile* term_file) const {

            }

            DataType operator()(const NodeTermCtEval* term_eval) const {
  
            }

            DataType operator()(const NodeTermCtMdefined* term_mdef) const {

            }

            DataType operator()(NodeTermSizeof* term_sizeof) const {

            }

            DataType operator()(const NodeTermRd* term_rd) const {

            }

            DataType operator()(const NodeTermCast* term_cast) const {
                sema.check_type_is_valid(term_cast->def, term_cast->type);
                return term_cast->type;
            }

            DataType operator()(const NodeTermUnref* term_unref) const {
                
            }

            DataType operator()(const NodeTermCastTo* term_cast_to) const {
   
            }

            DataType operator()(NodeTermTypeid* term_typeid) const {

            }

            DataType operator()(const NodeTermStrLit* term_str_lit) const {

            }

            DataType operator()(const NodeTermAmpersand* term_amp) const {

            }

            DataType operator()(const NodeTermDrvalue* term_drval) const {

            }

            DataType operator()(const NodeTermIdent* term_ident) const {

            }

            DataType operator()(const NodeTermParen* term_paren) const {

            }

            DataType operator()(NodeTermNmCall* term_call) const {

            }

            DataType operator()(NodeTermCall* term_call) const {

            }

            DataType operator()(const NodeTermMtCall* term_call) const {

            }
            DataType operator()(const NodeTermNmIdent* nm_ident) const {

            }
        };

        assert(base_expr != nullptr);

        TermVisitor visitor{ *this, base_expr, lvalue };
        return std::visit(visitor, term->var);
    }

    DataType analyze_bin_expr(const NodeBinExpr* bin_expr, NodeExpr* base_expr, bool lvalue = false)
    {
        struct BinExprVisitor {
            SemanticContext& sema;
            bool       lvalue;
            const NodeBinExpr* base;
            NodeExpr* base_expr;

            DataType operator()(const NodeBinExprSub* sub) const {
                return sema.analyze_default_bin_expr(sub, SemanticContext::DefBinaryOpKind::SUB, base, base_expr);
            }

            DataType operator()(const NodeBinExprAdd* add) const {
                return sema.analyze_default_bin_expr(add, SemanticContext::DefBinaryOpKind::ADD, base, base_expr);
            }

            DataType operator()(const NodeBinExprMulti* multi) const {
                return sema.analyze_default_bin_expr(multi, SemanticContext::DefBinaryOpKind::MUL, base, base_expr);
            }

            DataType operator()(const NodeBinExprDiv* div) const {
                return sema.analyze_default_bin_expr(div, SemanticContext::DefBinaryOpKind::DIV, base, base_expr);
            }

            DataType operator()(const NodeBinExprShl* shl) const {
                return sema.analyze_default_bin_expr(shl, SemanticContext::DefBinaryOpKind::SHL, base, base_expr);
            }

            DataType operator()(const NodeBinExprShr* shr) const {
                return sema.analyze_default_bin_expr(shr, SemanticContext::DefBinaryOpKind::SHR, base, base_expr);
            }

            DataType operator()(const NodeBinExprMod* md) const {
                return sema.analyze_default_bin_expr(md, SemanticContext::DefBinaryOpKind::MOD, base, base_expr);
            }

            DataType operator()(const NodeBinExprEqEq* eqeq) const {
                return sema.analyze_default_bin_expr(eqeq, SemanticContext::DefBinaryOpKind::EQUAL, base, base_expr, BaseDataTypeInt);
            }

            DataType operator()(const NodeBinExprNotEq* nq) const {
                return sema.analyze_default_bin_expr(nq, SemanticContext::DefBinaryOpKind::NOT_EQUAL, base, base_expr, BaseDataTypeInt);
            }

            DataType operator()(const NodeBinExprLess* less) const {
                return sema.analyze_default_bin_expr(less, SemanticContext::DefBinaryOpKind::LESS, base, base_expr, BaseDataTypeInt);
            }

            DataType operator()(const NodeBinExprAnd* band) const {
                return sema.analyze_default_bin_expr(band, SemanticContext::DefBinaryOpKind::LOGIC_AND, base, base_expr, BaseDataTypeInt); 
            }

            DataType operator()(const NodeBinExprOr* bor) const {
                return sema.analyze_default_bin_expr(bor, SemanticContext::DefBinaryOpKind::LOGIC_OR, base, base_expr, BaseDataTypeInt);
            }

            DataType operator()(const NodeBinExprAbove* above) const {
                return sema.analyze_default_bin_expr(above, SemanticContext::DefBinaryOpKind::ABOVE, base, base_expr, BaseDataTypeInt);
            }

            DataType operator()(const NodeBinExprDot* dot) const {
                DataType object_type = sema.analyze_expr(dot->lhs);

                if(!std::holds_alternative<NodeTerm*>(dot->rhs->var)) {
                    sema.m_diag_man->DiagnosticMessage(base->def, "error", "after `.` except identificator", 0);
                    exit(EXIT_FAILURE);
                }

                NodeTerm* term = std::get<NodeTerm*>(dot->rhs->var);

                if(!std::holds_alternative<NodeTermCall*>(term->var)) {
                    sema.m_diag_man->DiagnosticMessage(base->def, "error", "after `.` except identificator", 0);
                    exit(EXIT_FAILURE);
                }

                NodeTermCall* call = std::get<NodeTermCall*>(term->var);

                const std::string& method_name = call->name;

                if(!object_type.is_object()) sema.m_diag_man->DiagnosticMessage(base->def, "error", "you cannot call the `" + method_name + "` method on a non-object type `" + object_type.to_string() + "`.", 0);
            
                NodeTermMtCall* method_call = sema.m_allocator->emplace<NodeTermMtCall>();
                method_call->def = call->def;
                method_call->mt = dot->lhs;
                method_call->name = method_name;
            }

            DataType operator()(const NodeBinExprArgs* args) const {

            }
            DataType operator()(const NodeBinExprIndex* idx) const {

            }
        };

        assert(base_expr != nullptr);

        BinExprVisitor visitor{ *this, lvalue, bin_expr, base_expr };
        return std::visit(visitor, bin_expr->var);
    }

    DataType analyze_expr(NodeExpr* expr, bool lvalue = false)
    {
        struct ExprVisitor {
            SemanticContext& sema;
            bool       lvalue;
            NodeExpr* base_expr;

            DataType operator()(const NodeTerm* term) const
            {
                return sema.analyze_term(term, base_expr, lvalue);
            }

            DataType operator()(const NodeBinExpr* bin_expr) const
            {
                return sema.analyze_bin_expr(bin_expr, base_expr, lvalue);
            }
        };

        ExprVisitor visitor{ *this, lvalue, expr };
        return std::visit(visitor, expr->var);
    }

    void analyze_if_pred(const NodeIfPred* pred)
    {
        struct PredVisitor {
            SemanticContext& sema;

            void operator()(const NodeIfPredElif* elif) const
            {

            }

            void operator()(const NodeIfPredElse* else_) const
            {

            }
        };

        PredVisitor visitor{ *this };
        std::visit(visitor, pred->var);
    }

    std::vector<NodeExpr*> __getargs(NodeExpr* __expr) {
        return std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(__expr->var)->var)->args;
    }

    NodeBinExprArgs* getargs(NodeExpr* __expr) {
        return std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(__expr->var)->var);
    }

    bool types_equ(const DataType& one, const DataType& two) {
        return one.is_compatible_with(two);
    }

    bool procs_same_signature(const std::vector<std::pair<std::string, DataType>>& one,
                                const std::vector<std::pair<std::string, DataType>>& two)
    {
        if(one.size() != two.size()) return false;

        int size = static_cast<int>(one.size());

        for(int i = 0; i < size; i++) {
            if(!types_equ(one[i].second, two[i].second)) {
                return false;
            }
        }
        return true;
    }
    void analyze_scope(NodeScope* scope) {
        for(NodeStmt* stmt : scope->stmts) {
            analyze_stmt(stmt);
        }
    }

    void analyze_stmt(NodeStmt* stmt)
    {
        struct StmtVisitor {
            SemanticContext& sema;
            NodeStmt* base_stmt;

            void operator()(const NodeStmtExit* stmt_exit) const
            {   
                const DataType& expression_type = sema.analyze_expr(stmt_exit->expr);
                if(!sema.types_equ(expression_type, BaseDataTypeInt)) {
                    sema.m_diag_man->DiagnosticMessage(stmt_exit->def, "error", "exit expects type `int`, but type `" + expression_type.to_string() + "` was received.", 6);
                    exit(EXIT_FAILURE);
                }
            }

            void operator()(NodeStmtProc* stmt_proc)
            {
                const std::string& name = stmt_proc->name;
                if(sema.m_cur_procedure != nullptr) {
                    sema.m_diag_man->DiagnosticMessage(stmt_proc->def, "error", "defining a procedure within another procedure is prohibited", 0);
                    sema.m_diag_man->DiagnosticMessage(sema.m_cur_procedure->def, "note", "you are trying to define the `" + name + "` procedure while in the`" + sema.m_cur_procedure->name + "` procedure.", 0);
                    exit(EXIT_FAILURE);
                }

                SemanticScope* current_scope = nullptr;
                std::optional<Procedure*> existing_procedure = sema.m_sym_table.proc_lookup(name);
                
                if(sema.m_cur_namespace != nullptr) {
                    assert(sema.m_cur_namespace->scope != nullptr);
                    current_scope = sema.m_cur_namespace->scope;
                } else {
                    current_scope = sema.m_sym_table.last_scope();
                }

                Procedure* cproc = sema.m_allocator->emplace<Procedure>();
                cproc->name = name;
                cproc->params = stmt_proc->params;
                cproc->rettype = stmt_proc->rettype;
                cproc->stack_allign = 0;
                cproc->attrs = stmt_proc->attrs;
                cproc->def = stmt_proc->def;
                cproc->overload = false;
                cproc->prototype = stmt_proc->prototype;
                cproc->templates = stmt_proc->templates;
                cproc->from = stmt_proc;

                if(existing_procedure.has_value()) {
                    Procedure* existing_proc = existing_procedure.value();
                    bool signature_equals = sema.procs_same_signature(existing_proc->params, stmt_proc->params);
                    if(signature_equals && !stmt_proc->prototype) {
                        sema.m_diag_man->DiagnosticMessage(stmt_proc->def, "error", "precedure `" + name + "` redefenition", 0);
                        sema.m_diag_man->DiagnosticMessage(existing_proc->def, "note", "first defenition here.", 0);
                        exit(EXIT_FAILURE);
                    } else if(signature_equals && stmt_proc->prototype) {
                        current_scope->m_procs[name] = cproc;
                    } else {
                        existing_proc->overloads.push_back(cproc);
                    }
                } else {
                    current_scope->m_procs[name] = cproc;
                }

                if(stmt_proc->prototype || stmt_proc->templates != nullptr) return;

                sema.m_cur_procedure = cproc;
                sema.m_sym_table.begin_scope();
                sema.analyze_scope(stmt_proc->scope);
                sema.m_sym_table.end_scope();
                sema.m_cur_procedure = nullptr;
            }

            void operator()(const NodeStmtReturn* stmt_return) const
            {
                if(sema.m_cur_procedure == nullptr) {
                    sema.m_diag_man->DiagnosticMessage(stmt_return->def, "error", "return outside the procedure body.", 0);
                    exit(EXIT_FAILURE);
                }

                Procedure* current_proc = sema.m_cur_procedure;

                if(!stmt_return->expr.has_value() && current_proc->rettype != BaseDataTypeVoid) {
                    sema.m_diag_man->DiagnosticMessage(current_proc->def, "error", "procedure `" + current_proc->name + "` expects a return type of `" + current_proc->rettype.to_string() + "`", 0);
                    sema.m_diag_man->DiagnosticMessage(stmt_return->def, "note", "but the type `void` was received.", 0);
                    exit(EXIT_FAILURE);
                }

                if(stmt_return->expr.has_value()) {
                    const DataType& expression_type = sema.analyze_expr(stmt_return->expr.value());
                    if(!sema.types_equ(expression_type, current_proc->rettype)) {
                        sema.m_diag_man->DiagnosticMessage(current_proc->def, "error", "procedure `" + current_proc->name + "` expects a return type of `" + current_proc->rettype.to_string() + "`", 0);
                        sema.m_diag_man->DiagnosticMessage(stmt_return->def, "note", "but the type `" + expression_type.to_string() + "` was received.", 0);
                        exit(EXIT_FAILURE);
                    }
                }
            }

            void operator()(const NodeStmtLet* stmt_let) const
            {
                assert(stmt_let->ident.value.has_value());
                
                const std::string& name = stmt_let->ident.value.value();
                DataType expression_type = sema.analyze_expr(stmt_let->expr);
                std::optional<DataType> explicit_type = stmt_let->type;

                // TODO: NodeStmtLetNoAssign can be deleted, and here expr can be optional, for reduce code size.
                
                if(explicit_type.has_value()) {
                    const DataType& explicit_type_unpacked = explicit_type.value();
                    if(!sema.types_equ(explicit_type_unpacked, expression_type)) {
                        sema.m_diag_man->DiagnosticMessage(stmt_let->ident, "error", "when defining the variable, the expected type was `" + explicit_type_unpacked.to_string() + "`, but the type `" + expression_type.to_string() + "` was obtained.", name.length() + 2);
                        exit(EXIT_FAILURE);
                    }
                }

                if(sema.m_sym_table.m_scopes.size() == 1) { // means that this gonna be global variable
                    GlobalVariable* to_insert = sema.m_allocator->alloc<GlobalVariable>();
                    to_insert->name = name;
                    to_insert->type = expression_type;
                    to_insert->mangled_symbol = "_v_" + name;
                    sema.m_sym_table.m_gvars[name] = to_insert;
                } else {
                    if(sema.m_cur_namespace != nullptr && sema.m_cur_procedure == nullptr) {
                        GlobalVariable* to_insert = sema.m_allocator->alloc<GlobalVariable>();
                        to_insert->name = name;
                        to_insert->type = expression_type;
                        std::string mangl = "_v_" + name;
                        Namespace* current = sema.m_cur_namespace;
                        while(current != nullptr) {
                            mangl = current->name + "@" + mangl;
                            current = current->parent;
                        }
                        to_insert->mangled_symbol = mangl;
                        sema.m_cur_namespace->scope->m_gvars[name] = to_insert;
                    } else {
                        assert(sema.m_cur_procedure != nullptr);
                        Variable* to_insert = sema.m_allocator->alloc<Variable>();
                        to_insert->name = name;
                        to_insert->type = expression_type;
                        sema.m_cur_procedure->stack_allign += 4;
                        to_insert->stack_loc = sema.m_cur_procedure->stack_allign;
                        sema.m_sym_table.last_scope()->m_vars[name] = to_insert;
                    }
                }
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

            }

            void operator()(const NodeStmtPushOnStack* stmt_push) const
            {

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

        StmtVisitor visitor{ *this , stmt };
        std::visit(visitor, stmt->var);
    }

    void analyze_prog()
	{
        m_sym_table.begin_scope();
        for (NodeStmt* stmt : m_prog->stmts) {
            analyze_stmt(stmt);
        }
	}

private:
    NodeProg* m_prog;
    
    std::unordered_map<std::string, bool> m_validated_types;

    std::unordered_map<DefBinaryOpKind, std::pair<const char*, bool>> m_error_message_by_binop_kind = {
        {DefBinaryOpKind::SUB, {"you cannot subtract the `%s` type from the `%s` type.", true}},
        {DefBinaryOpKind::ADD, {"you cannot add two different types, `%s` and `%s`, together.", false}},
        {DefBinaryOpKind::MUL, {"you cannot multiply two different types, `%s` and `%s`, together.", false}},
        {DefBinaryOpKind::DIV, {"you cannot divide an `%s` type by a `%s` type.", false}},
        {DefBinaryOpKind::SHL, {"you cannot left shift an `%s` type by a `%s` type.", false}},
        {DefBinaryOpKind::SHR, {"you cannot right shift an `%s` type by a `%s` type.", false}},
        {DefBinaryOpKind::MOD, {"you cannot modulo an `%s` type by a `%s` type.", false}},
        {DefBinaryOpKind::EQUAL, {"you cannot compare two different types, `%s` and `%s`.", false}},
        {DefBinaryOpKind::NOT_EQUAL, {"you cannot compare two different types, `%s` and `%s`.", false}},
        {DefBinaryOpKind::LESS, {"you cannot compare two different types, `%s` and `%s`.", false}},
        {DefBinaryOpKind::LOGIC_AND, {"you cannot logically AND two different types, `%s` and `%s`.", false}},
        {DefBinaryOpKind::LOGIC_OR, {"you cannot logically OR two different types, `%s` and `%s`.", false}},
        {DefBinaryOpKind::ABOVE, {"you cannot compare two different types, `%s` and `%s`.", false}},
    };

    std::unordered_map<DefBinaryOpKind, const char*> m_method_name_by_binop_kind = {
        {DefBinaryOpKind::SUB, "m_sub"},
        {DefBinaryOpKind::ADD, "m_add"},
        {DefBinaryOpKind::MUL, "m_mul"},
        {DefBinaryOpKind::DIV, "m_div"},
        {DefBinaryOpKind::SHL, "m_shl"},
        {DefBinaryOpKind::SHR, "m_shr"},
        {DefBinaryOpKind::MOD, "m_mod"},
        {DefBinaryOpKind::EQUAL, "m_equal"},
        {DefBinaryOpKind::NOT_EQUAL, "m_not_equal"},
        {DefBinaryOpKind::LESS, "m_less"},
        {DefBinaryOpKind::LOGIC_AND, "m_logic_and"},
        {DefBinaryOpKind::LOGIC_OR, "m_logic_or"},
        {DefBinaryOpKind::ABOVE, "m_above"},
    };
};