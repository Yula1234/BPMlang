#pragma once

struct Namespace;

struct Procedure {
	GString name;
	GVector<std::pair<GString, DataType>> params;
	DataType rettype;
	size_t stack_allign;
	GVector<ProcAttr> attrs;
	Token def;
	bool prototype;
	bool overload;
	GVector<GString>* templates;
    NodeStmtProc* from;
    GString mangled_symbol;
    GVector<Procedure*> overloads;
    Namespace* nmspace;
    GString return_label;
};

struct Variable {
    GString name;
    DataType type;
    size_t stack_loc;
};

struct GlobalVariable {
    GString name;
    DataType type;
    GString mangled_symbol;
};

struct Struct {
    GString name;
    GMap<GString, std::pair<DataType, int>> fields; // name => pair<Type, Position in Order>
    size_t _typeid;
    GVector<GString> temps;
    Token def;
    std::optional<DataType> parent_type;
};

struct Interface {
    GString name;
    GMap<GString, std::pair<Procedure*, size_t>> methods;
    size_t _typeid;
    
};

namespace std {
template<>
struct hash<DataType> {
    size_t operator()(const DataType& p) const noexcept {
        std::hash<const char*> h;
        return h(p.to_string().c_str());
    }
};
}

class SemanticScope;

struct Namespace {
	GString name;
	SemanticScope* scope = nullptr;
	Namespace* parent;

    GString get_mangle() {
        GString mangl = name;
        Namespace* current = parent;
        while(current != nullptr) {
            mangl = current->name + "@" + mangl;
            current = current->parent;
        }
        return mangl + "@";
    }

    GString get_path() {
        GString mangl = name;
        Namespace* current = parent;
        while(current != nullptr) {
            mangl = current->name + "::" + mangl;
            current = current->parent;
        }
        return mangl + "::";
    }
};


class SemanticScope {
public:
    GMap<GString, Procedure*> m_procs;
    GMap<GString, Struct*> m_structs;
    GMap<GString, Variable*> m_vars;
    GMap<GString, Namespace*> m_namespaces;
    GMap<GString, GlobalVariable*> m_gvars;

    std::optional<Procedure*> proc_lookup(const GString& name) const {
        const auto& search = m_procs.find(name);
        if(search != m_procs.end()) return search->second;
        return std::nullopt;
    }
    std::optional<Variable*> var_lookup(const GString& name) const {
        const auto& search = m_vars.find(name);
        if(search != m_vars.end()) return search->second;
        return std::nullopt;
    }
    std::optional<Namespace*> namespace_lookup(const GString& name) const {
        const auto& search = m_namespaces.find(name);
        if(search != m_namespaces.end()) return search->second;
        return std::nullopt;
    }
    std::optional<Struct*> struct_lookup(const GString& name) const {
        const auto& search = m_structs.find(name);
        if(search != m_structs.end()) return search->second;
        return std::nullopt;
    }
    std::optional<GlobalVariable*> gvar_lookup(const GString& name) const {
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


enum class TermIdentSymbolKind {
    GLOBAL_VAR,
    LOCAL_VAR,
};

struct TermIdentSymbol {
    TermIdentSymbolKind kind;
    void* symbol;
};



class SemanticSymbolTable {
public:
    GMap<GString, GlobalVariable*> m_gvars;
    
    GDeque<SemanticScope*> m_scopes;

    ArenaAllocator* m_allocator;

    GMap<NodeStmtProc*, Procedure*> m_mapped_procs_symbols;
    GMap<NodeStmtReturn*, Procedure*> m_mapped_return_symbols;
    GMap<NodeTermIdent*, TermIdentSymbol> m_mapped_ident_symbols;
    GMap<NodeTermCall*, Procedure*> m_mapped_calls_symbols;
    GMap<NodeStmtLet*, TermIdentSymbol> m_mapped_let_symbols;

	SemanticSymbolTable() = default;

    std::optional<Procedure*> proc_lookup(const GString& name) {
        for(const SemanticScope* scope : m_scopes) {
            std::optional<Procedure*> res = scope->proc_lookup(name);
            if(res.has_value()) return res;
        }
        return std::nullopt;
    }
    std::optional<Variable*> var_lookup(const GString& name) {
        for(const SemanticScope* scope : m_scopes) {
            std::optional<Variable*> res = scope->var_lookup(name);
            if(res.has_value()) return res;
        }
        return std::nullopt;
    }
    std::optional<Namespace*> namespace_lookup(const GString& name) {
        for(const SemanticScope* scope : m_scopes) {
            std::optional<Namespace*> res = scope->namespace_lookup(name);
            if(res.has_value()) return res;
        }
        return std::nullopt;
    }
    std::optional<Struct*> struct_lookup(const GString& name) {
        for(const SemanticScope* scope : m_scopes) {
            std::optional<Struct*> res = scope->struct_lookup(name);
            if(res.has_value()) return res;
        }
        return std::nullopt;
    }
    std::optional<GlobalVariable*> gvar_lookup(const GString& name) {
        const auto& search = m_gvars.find(name);
        if(search != m_gvars.end()) return search->second;
        return std::nullopt;
    }
    inline SemanticScope* last_scope() {
        assert(!m_scopes.empty());
        return m_scopes[0];
    }
    void begin_scope() {
        m_scopes.push_front(m_allocator->emplace<SemanticScope>());
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

    Procedure* m_entry_point = nullptr;

    DiagnosticManager* m_diag_man;
    ArenaAllocator* m_allocator;

	explicit SemanticContext(NodeProg* prog, DiagnosticManager* dman, ArenaAllocator* arena) : m_template_instantiator(arena, dman, *this) {
		m_diag_man = dman;
		m_prog = prog;
		m_allocator = arena;
        m_sym_table.m_allocator = arena;
	}

    template <typename T>
    NodeExpr* as_expr_pointer(T term_or_bin_expr) {
        NodeExpr* as_expr = m_allocator->emplace<NodeExpr>();
        as_expr->var = term_or_bin_expr;
        return as_expr;
    }

    template <typename T>
    NodeTerm* as_term_pointer(T term) {
        NodeTerm* as_term = m_allocator->emplace<NodeTerm>();
        as_term->var = term;
        return as_term;
    }

    NodeExpr* construct_args_from_vector(const Token& def, const GVector<NodeExpr*>& args) {
        NodeBinExprArgs* bin_expr_args = m_allocator->emplace<NodeBinExprArgs>();
        bin_expr_args->args = args;
        NodeBinExpr* bin_expr = m_allocator->emplace<NodeBinExpr>();
        bin_expr->var = bin_expr_args;
        bin_expr->def = def;
        return as_expr_pointer(bin_expr);
    }

    NodeExpr* construct_method_call(const Token& def, NodeExpr* object, const GString& name,
                                    const GVector<NodeExpr*>& args, const GVector<DataType>& targs)
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

        const GString& object_name = type.getobjectname();

        const auto& search = m_validated_types.find(object_name);

        if(search != m_validated_types.end()) return;

        if(!m_sym_table.struct_lookup(object_name).has_value()) {
            m_diag_man->DiagnosticMessage(def, "error", "undefined type `" + object_name + "`", 0);
            exit(EXIT_FAILURE);
        }

        m_validated_types[object_name] = true;
    }

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
            GString sname = type.root().getobjectname();
            std::optional<Struct*> st = m_sym_table.struct_lookup(sname);
            if (st.has_value())  return st.value()->_typeid;
        }
        assert(false);
        return 0;
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
            GVector<NodeExpr*> prepared_args({bin_expr->lhs, bin_expr->rhs});
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

    GVector<NodeExpr*> __getargs(NodeExpr* __expr) {
        return std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(__expr->var)->var)->args;
    }

    NodeBinExprArgs* getargs(NodeExpr* __expr) {
        return std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(__expr->var)->var);
    }

    bool types_equ(const DataType& one, const DataType& two) {
        return one.is_compatible_with(two);
    }

    bool procs_same_signature(const GVector<std::pair<GString, DataType>>& one,
                                const GVector<std::pair<GString, DataType>>& two,
                                const DataType& one_rettype, const DataType& two_rettype)
    {
        if(one_rettype != two_rettype) return false;
        if(one.size() != two.size()) return false;

        int size = static_cast<int>(one.size());

        for(int i = 0; i < size; i++) {
            if(!types_equ(one[i].second, two[i].second)) {
                return false;
            }
        }
        return true;
    }

    std::pair<bool, size_t> match_call_signature(const GVector<DataType>& args,
                              const GVector<std::pair<GString, DataType>>& params,
                              Procedure* proc)
    {
        bool nosizedargs = std::find(proc->attrs.begin(), proc->attrs.end(), ProcAttr::nosizedargs) != proc->attrs.end();
        if (args.size() != params.size() && !nosizedargs) return {false, 0};
        if (args.size() <  params.size() &&  nosizedargs) return {false, 0};

        for(size_t i = 0;i < params.size();i += 1) {
            DataType argtype = args[i];
            DataType excepted_type = params[i].second;
            if(!types_equ(excepted_type, argtype)) {
                return {false, i};
            }
        }

        return {true, 0};
    }

    Procedure* resolve_overloading(const Token& def, Procedure* procedure,
                                    GVector<DataType> args, [[maybe_unused]] GVector<DataType> template_args)
    {
        std::pair<bool, size_t> first_result = match_call_signature(args, procedure->params, procedure);
        if(first_result.first) return procedure;

        Procedure* result_proc = nullptr;

        GVector<Procedure*> candidates {procedure};

        for(size_t i = 0;i < procedure->overloads.size();i++) {
            Procedure* current = procedure->overloads[i];
            candidates.push_back(current);
            std::pair<bool, size_t> matches = match_call_signature(args, current->params, current);
            if(matches.first) {
                result_proc = current;
                break;
            }
        }

        if(result_proc == nullptr) {
            GString call_signature = procedure->name + "(";
            if(!args.empty()) {
                for(size_t i = 0;i < args.size();i++) {
                    call_signature += args[i].to_string();
                    if(i != args.size() - 1) {
                        call_signature += ", ";
                    }
                }
            }
            call_signature += ")";
            m_diag_man->DiagnosticMessage(def, "error", "could not find candidates for calling the procedure " + call_signature + ".", 0);
            for(size_t i = 0;i < candidates.size();i++) {
                Procedure* current = candidates[i];
                GString candidate_signature = current->name + "(";
                if(!current->params.empty()) {
                    for(size_t j = 0;j < current->params.size();j++) {
                        candidate_signature += current->params[j].second.to_string();
                        if(j != current->params.size() - 1) {
                            candidate_signature += ", ";
                        }
                    }
                }
                candidate_signature += ")";
                m_diag_man->DiagnosticMessage(current->def, "note", "candidate " + candidate_signature, 0);
            }
            exit(EXIT_FAILURE);
        }

        return result_proc;
    }

    Variable* define_local_variable(const GString& name, const DataType& type) {
        assert(m_cur_procedure != nullptr);
        Variable* to_insert = m_allocator->alloc<Variable>();
        to_insert->name = name;
        to_insert->type = type;
        m_cur_procedure->stack_allign += 4;
        to_insert->stack_loc = m_cur_procedure->stack_allign;
        m_sym_table.last_scope()->m_vars[name] = to_insert;
        return to_insert;
    }

    DataType analyze_term(const NodeTerm* term, NodeExpr* base_expr, bool lvalue = false)
    {
        struct TermVisitor {
            SemanticContext& sema;
            NodeExpr* base_expr;
            bool       lvalue;

            DataType operator()(const NodeTermIntLit* term_int_lit) const {
                if (lvalue) {
                    sema.m_diag_man->DiagnosticMessage(term_int_lit->int_lit, "error", "you cannot use an int literal as an lvalue, or take its address.", 0);
                    exit(EXIT_FAILURE);
                }
                return BaseDataTypeInt;
            }

            DataType operator()(const NodeTermType* tp) {
                sema.m_diag_man->DiagnosticMessage(tp->def, "error", "the ct_type() expression can only be used in compile-time calculations.", 0);
                exit(EXIT_FAILURE);
            }

            DataType operator()([[maybe_unused]] const NodeTermCol* term_col) const {
                return BaseDataTypeInt;
            }

            DataType operator()([[maybe_unused]] const NodeTermLine* term_line) const {
                return BaseDataTypeInt;
            }

            DataType operator()([[maybe_unused]] const NodeTermPop* term_pop) const {
                return BaseDataTypeInt;
            }

            DataType operator()(const NodeTermExprStmt* term_stmt) const {
                sema.m_sym_table.begin_scope();
                sema.analyze_scope(term_stmt->scope);
                sema.m_sym_table.end_scope();
                return sema.analyze_expr(term_stmt->expr);
            }

            DataType operator()([[maybe_unused]] const NodeTermFile* term_file) const {
                BaseDataType rtype = BaseDataTypeChar;
                rtype.ptrlvl += 1;
                return rtype;
            }

            DataType operator()([[maybe_unused]] const NodeTermCtEval* term_eval) const {
                return BaseDataTypeVoid;
            }

            DataType operator()([[maybe_unused]] const NodeTermCtMdefined* term_mdef) const {
                return BaseDataTypeVoid;
            }

            DataType operator()([[maybe_unused]] const NodeTermSizeof* term_sizeof) const {
                return BaseDataTypeVoid;
            }

            DataType operator()([[maybe_unused]] const NodeTermRd* term_rd) const {
                return BaseDataTypeVoid;
            }

            DataType operator()(const NodeTermCast* term_cast) const {
                sema.check_type_is_valid(term_cast->def, term_cast->type);
                return term_cast->type;
            }

            DataType operator()([[maybe_unused]] const NodeTermUnref* term_unref) const {
                return BaseDataTypeVoid;
            }

            DataType operator()([[maybe_unused]] const NodeTermCastTo* term_cast_to) const {
                return BaseDataTypeVoid;
            }

            DataType operator()([[maybe_unused]] const NodeTermTypeid* term_typeid) const {
                return BaseDataTypeVoid;
            }

            DataType operator()([[maybe_unused]] const NodeTermStrLit* term_str_lit) const {
                return BaseDataTypeVoid;
            }

            DataType operator()([[maybe_unused]] const NodeTermAmpersand* term_amp) const {
                return BaseDataTypeVoid;
            }

            DataType operator()([[maybe_unused]] const NodeTermDrvalue* term_drval) const {
                return BaseDataTypeVoid;
            }

            DataType operator()(NodeTermIdent* term_ident) const {
                const GString& name = term_ident->ident.value.value();

                std::optional<Variable*> variable = sema.m_sym_table.var_lookup(name);
                if(variable.has_value()) {
                    sema.m_sym_table.m_mapped_ident_symbols[term_ident] = TermIdentSymbol {
                        TermIdentSymbolKind::LOCAL_VAR, reinterpret_cast<void*>(variable.value())
                    };
                    return variable.value()->type;
                }
                std::optional<GlobalVariable*> global_variable = sema.m_sym_table.gvar_lookup(name);
                if(global_variable.has_value()) {
                    sema.m_sym_table.m_mapped_ident_symbols[term_ident] = TermIdentSymbol {
                        TermIdentSymbolKind::GLOBAL_VAR, reinterpret_cast<void*>(global_variable.value())
                    };
                    return global_variable.value()->type;
                }

                sema.m_diag_man->DiagnosticMessage(term_ident->ident, "error", "undefined symbol `" + name + "`", 0);
                exit(EXIT_FAILURE);
            }

            DataType operator()([[maybe_unused]] const NodeTermParen* term_paren) const {
                return BaseDataTypeVoid;
            }

            DataType operator()([[maybe_unused]] const NodeTermNmCall* term_call) const {
                return BaseDataTypeVoid;
            }

            DataType operator()(NodeTermCall* term_call) const {
                const GString& name = term_call->name;
                std::optional<Procedure*> _procedure = sema.m_sym_table.proc_lookup(name);

                if(!_procedure.has_value()) {
                    sema.m_diag_man->DiagnosticMessage(term_call->def, "error", "undefined procedure `" + name + "`", 0);
                    exit(EXIT_FAILURE);
                }

                Procedure* proc = _procedure.value();

                GVector<NodeExpr*> args{};

                if(term_call->args.has_value()) {
                    args = sema.__getargs(term_call->args.value());
                }

                GVector<DataType> args_types;
                for(size_t i = 0;i < args.size();i++) {
                    args_types.push_back(sema.analyze_expr(args[i]));
                }

                if(proc->templates != NULL) {
                    proc = sema.m_template_instantiator.instantiate_procedure(proc, term_call->targs, term_call->def);
                }

                if(!proc->overloads.empty()) {
                    proc = sema.resolve_overloading(term_call->def, proc, args_types, term_call->targs);
                } else {
                    std::pair<bool, size_t> typecheck_result = sema.match_call_signature(args_types, proc->params, proc);
                    if(!typecheck_result.first) {
                        sema.m_diag_man->DiagnosticMessage(proc->def, "error", "procedure `" + proc->name + "` expects an `" + proc->params[typecheck_result.second].second.to_string() + "` " + GString(std::to_string(typecheck_result.second + 1).c_str()) + " argument type", 0);
                        sema.m_diag_man->DiagnosticMessage(term_call->def, "note", "but got `" + args_types[typecheck_result.second].to_string() + "`", 0);
                        exit(EXIT_FAILURE);
                    }
                }

                sema.m_sym_table.m_mapped_calls_symbols[term_call] = proc;

                return proc->rettype;
            }

            DataType operator()([[maybe_unused]] const NodeTermMtCall* term_call) const {
                return BaseDataTypeVoid;
            }
            DataType operator()([[maybe_unused]] const NodeTermNmIdent* nm_ident) const {
                return BaseDataTypeVoid;
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

                const GString& method_name = call->name;

                if(!object_type.is_object()) sema.m_diag_man->DiagnosticMessage(base->def, "error", "you cannot call the `" + method_name + "` method on a non-object type `" + object_type.to_string() + "`.", 0);
            
                NodeTermMtCall* method_call = sema.m_allocator->emplace<NodeTermMtCall>();
                method_call->def = call->def;
                method_call->mt = dot->lhs;
                method_call->name = method_name;
                return BaseDataTypeVoid;
            }

            DataType operator()([[maybe_unused]] const NodeBinExprArgs* args) const {
                return BaseDataTypeVoid;
            }
            DataType operator()([[maybe_unused]] const NodeBinExprIndex* idx) const {
                return BaseDataTypeVoid;
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

            void operator()([[maybe_unused]] const NodeIfPredElif* elif) const
            {

            }

            void operator()([[maybe_unused]] const NodeIfPredElse* else_) const
            {

            }
        };

        PredVisitor visitor{ *this };
        std::visit(visitor, pred->var);
    }

    void analyze_scope(NodeScope* scope) {
        for(NodeStmt* stmt : scope->stmts) {
            analyze_stmt(stmt);
        }
    }

    Procedure* construct_procedure_from_stmt(NodeStmtProc* stmt_proc) {
        Procedure* cproc = m_allocator->emplace<Procedure>();
        m_sym_table.m_mapped_procs_symbols[stmt_proc] = cproc;
        cproc->name = stmt_proc->name;
        cproc->params = stmt_proc->params;
        cproc->rettype = stmt_proc->rettype;
        cproc->stack_allign = 0;
        cproc->attrs = stmt_proc->attrs;
        cproc->def = stmt_proc->def;
        cproc->overload = false;
        cproc->prototype = stmt_proc->prototype;
        cproc->templates = stmt_proc->templates;
        cproc->from = stmt_proc;

        GString mangled_symbol = stmt_proc->name;
        for(size_t i = 0ULL;i < stmt_proc->params.size();i += 1) {
            mangled_symbol += stmt_proc->params[i].second.sign();
        }
        mangled_symbol += "@" + std::to_string(stmt_proc->params.size());
        cproc->mangled_symbol = mangled_symbol;
        cproc->nmspace = m_cur_namespace;
        return cproc;
    }

    void analyze_procedure(Procedure* procedure) {
        if(procedure->nmspace != nullptr) {
            procedure->mangled_symbol = procedure->nmspace->get_mangle() + procedure->mangled_symbol;
            procedure->name = procedure->nmspace->get_path() + procedure->from->name;
        }

        if(procedure->from->prototype || procedure->from->templates != nullptr) return;

        m_cur_procedure = procedure;
        m_sym_table.begin_scope();
        for(size_t i = 0;i < procedure->params.size();i++) {
            define_local_variable(procedure->params[i].first, procedure->params[i].second);
        }
        analyze_scope(procedure->from->scope);
        m_sym_table.end_scope();
        m_cur_procedure = nullptr;
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
                const GString& name = stmt_proc->name;
                if(sema.m_cur_procedure != nullptr) {
                    sema.m_diag_man->DiagnosticMessage(stmt_proc->def, "error", "defining a procedure within another procedure is prohibited", 0);
                    sema.m_diag_man->DiagnosticMessage(sema.m_cur_procedure->def, "note", "you are trying to define the `" + name + "` procedure while in the `" + sema.m_cur_procedure->name + "` procedure.", 0);
                    exit(EXIT_FAILURE);
                }

                SemanticScope* current_scope = sema.m_sym_table.last_scope();
                std::optional<Procedure*> existing_procedure = current_scope->proc_lookup(name);

                Procedure* cproc = sema.construct_procedure_from_stmt(stmt_proc);

                if(name == "main" && sema.m_sym_table.m_scopes.size() == 1 && sema.m_cur_namespace == nullptr) sema.m_entry_point = cproc;

                if(existing_procedure.has_value()) {
                    Procedure* existing_proc = existing_procedure.value();
                    bool signature_equals = sema.procs_same_signature(existing_proc->params, stmt_proc->params, existing_proc->rettype, stmt_proc->rettype);
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

                sema.analyze_procedure(cproc);
            }

            void operator()(NodeStmtReturn* stmt_return) const
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

                sema.m_sym_table.m_mapped_return_symbols[stmt_return] = current_proc;
            }

            void operator()(NodeStmtLet* stmt_let) const
            {
                assert(stmt_let->ident.value.has_value());
                
                const GString& name = stmt_let->ident.value.value();
                DataType expression_type {};
                std::optional<DataType> explicit_type = stmt_let->type;
                if(stmt_let->expr.has_value()) {
                    expression_type = sema.analyze_expr(stmt_let->expr.value());
                } else if(explicit_type.has_value()) {
                    expression_type = explicit_type.value();
                } else {
                    assert(false);
                }

                // TODO: NodeStmtLetNoAssign can be deleted, and here expr can be optional, for reduce code size.
                
                if(explicit_type.has_value() && stmt_let->expr.has_value()) {
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

                    sema.m_sym_table.m_mapped_let_symbols[stmt_let] = TermIdentSymbol {
                        TermIdentSymbolKind::GLOBAL_VAR, reinterpret_cast<void*>(to_insert)
                    };
                } else {
                    if(sema.m_cur_namespace != nullptr && sema.m_cur_procedure == nullptr) {
                        GlobalVariable* to_insert = sema.m_allocator->alloc<GlobalVariable>();
                        to_insert->name = name;
                        to_insert->type = expression_type;
                        to_insert->mangled_symbol = sema.m_cur_namespace->get_mangle() + "_v@" + name;
                        sema.m_cur_namespace->scope->m_gvars[name] = to_insert;
                        sema.m_sym_table.m_mapped_let_symbols[stmt_let] = TermIdentSymbol {
                            TermIdentSymbolKind::GLOBAL_VAR, reinterpret_cast<void*>(to_insert)
                        };
                    } else {
                        sema.m_sym_table.m_mapped_let_symbols[stmt_let] = TermIdentSymbol {
                            TermIdentSymbolKind::LOCAL_VAR, reinterpret_cast<void*>(sema.define_local_variable(name, expression_type))
                        };
                    }
                }
            }

            void operator()([[maybe_unused]] const NodeStmtCompileTimeIf* stmt_ctif) const {

            }

            void operator()([[maybe_unused]] const NodeStmtAssign* stmt_assign) const
            {

            }

            void operator()([[maybe_unused]] const NodeStmtIncBy* stmt_assign) const
            {

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
                NodeTermCall* call = sema.m_allocator->emplace<NodeTermCall>();
                call->def = stmt_call->def;
                call->name = stmt_call->name;
                call->args = stmt_call->args;
                call->targs = stmt_call->targs;
                call->as_expr = false;
                stmt_call->resolved_expression = sema.as_expr_pointer(sema.as_term_pointer(call));
                sema.analyze_expr(stmt_call->resolved_expression);
            }

            void operator()([[maybe_unused]] const NodeScope* scope) const
            {

            }

            void operator()([[maybe_unused]] const NodeStmtPushOnStack* stmt_push) const
            {

            }

            void operator()([[maybe_unused]] const NodeStmtIf* stmt_if) const
            {

            }

            void operator()([[maybe_unused]] const NodeStmtWhile* stmt_while) const
            {

            }

            void operator()([[maybe_unused]] const NodeStmtBreak* stmt_break) const
            {

            }

            void operator()([[maybe_unused]] const NodeStmtStore* stmt_store) const
            {

            }

            void operator()([[maybe_unused]] const NodeStmtBuffer* stmt_buf) const
            {

            }

            void operator()([[maybe_unused]] const NodeStmtAsm* stmt_asm) const
            {

            }

            void operator()([[maybe_unused]] const NodeStmtCextern* stmt_cextern) const
            {

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
                Namespace* old_namespace = sema.m_cur_namespace;

                Namespace* current_namespace = sema.m_allocator->emplace<Namespace>();
                current_namespace->parent = old_namespace;
                sema.m_sym_table.last_scope()->m_namespaces[stmt_space->name] = current_namespace;

                current_namespace->name = stmt_space->name;
                current_namespace->scope = sema.m_allocator->emplace<SemanticScope>();
                sema.m_cur_namespace = current_namespace;
                sema.m_sym_table.m_scopes.push_front(current_namespace->scope); // begin_scope()

                sema.analyze_scope(stmt_space->scope);

                sema.m_sym_table.end_scope();

                sema.m_cur_namespace = old_namespace;
            }

            void operator()([[maybe_unused]] const NodeStmtImpl* stmt_impl) const
            {
                
            }

            void operator()([[maybe_unused]] const NodeStmtNmCall* stmt_call) const
            {
                
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

        StmtVisitor visitor{ *this , stmt };
        std::visit(visitor, stmt->var);
    }

    void analyze_prog()
	{
        m_sym_table.begin_scope();
        
        for (NodeStmt* stmt : m_prog->stmts) {
            analyze_stmt(stmt);
        }
        
        if(m_entry_point == nullptr) {
            m_diag_man->DiagnosticMessage("error", "your program does not define the main function, which is required.");
            exit(EXIT_FAILURE);
        }

        while (!m_template_instantiator.m_pending_bodies.empty()) {
            auto item = m_template_instantiator.m_pending_bodies.front();
            m_template_instantiator.m_pending_bodies.pop_front();

            Procedure* proc = item.proc_symbol;
            NodeStmtProc* node = item.ast_node;

            analyze_procedure(proc);

            m_prog->stmts.push_back(m_allocator->emplace<NodeStmt>(node));
        }
	}

private:
    NodeProg* m_prog;
    
    GMap<GString, bool> m_validated_types;

    GMap<DefBinaryOpKind, std::pair<const char*, bool>> m_error_message_by_binop_kind = {
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

    GMap<DefBinaryOpKind, const char*> m_method_name_by_binop_kind = {
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

    class TemplateInstantiator {
    public:

        struct PendingBody {
            NodeStmtProc* ast_node;
            Procedure* proc_symbol;
        };

        GDeque<PendingBody> m_pending_bodies;
    
        ArenaAllocator* m_allocator = nullptr;
        DiagnosticManager* m_diag_man = nullptr;
        AstCloner m_ast_cloner;
        SemanticContext& m_sema;

        GMap<GString, Procedure*> m_instantiated_procs;
    
        explicit TemplateInstantiator(ArenaAllocator* arena, DiagnosticManager* dman, SemanticContext& sema) : m_ast_cloner(arena), m_sema(sema) {
            m_allocator = arena;
            m_diag_man = dman;
        }
    
        Procedure* instantiate_procedure(Procedure* template_procedure, const GVector<DataType>& template_args, const Token& call_site) {
            GString new_signature = template_procedure->name + "<";
            for(size_t i = 0;i < template_args.size();i++) {
                new_signature += template_args[i].to_string();
                if(i != template_args.size() - 1) {
                    new_signature += ", ";
                }
            }
            new_signature += ">";
    
            const auto& search = m_instantiated_procs.find(new_signature);
            if(search != m_instantiated_procs.end()) return search->second;
    
            if (template_procedure->templates->size() != template_args.size()) {
                m_diag_man->DiagnosticMessage(call_site, "error", 
                    "template types expects " + GString(std::to_string(template_procedure->templates->size()).c_str()) + 
                    " arguments, but got " + GString(std::to_string(template_args.size()).c_str()), 0);
                exit(EXIT_FAILURE);
            }
    
            GMap<GString, DataType> template_map;
            for (size_t i = 0; i < template_args.size(); ++i) {
                template_map[template_procedure->templates->at(i)] = template_args[i];
            }
    
            NodeStmt* clonned_base_stmt = m_ast_cloner.clone_stmt(m_allocator->emplace<NodeStmt>(template_procedure->from));
            NodeStmtProc* clonned_base = std::get<NodeStmtProc*>(clonned_base_stmt->var);
            clonned_base->templates = NULL;
    
            TypeSubstitutor substitutor(template_map);
    
            substitutor.substitute_stmt(clonned_base_stmt);

            Procedure* new_proc = m_sema.construct_procedure_from_stmt(clonned_base);
            new_proc->name = new_signature;

            m_sema.m_sym_table.m_mapped_procs_symbols[clonned_base] = new_proc;

            m_instantiated_procs[new_signature] = new_proc;

            m_pending_bodies.push_back({clonned_base, new_proc});

            return new_proc;
    
        }
    };

    TemplateInstantiator m_template_instantiator;
};