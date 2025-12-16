#pragma once

struct Namespace;

struct Procedure {
	GString name;
	GVector<std::pair<GString, DataType>> params;
	DataType rettype;
	size_t stack_allign = 0;
    size_t max_stack_allign = 0;
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
    GMap<GString, Field> fields;
    size_t _typeid;
    GVector<GString> temps;
    Token def;
    bool temp;
    std::optional<DataType> parent_type;
    GVector<std::pair<GString, DataType>> __fields;
    GMap<GVector<DataType>, Struct*> specializations;
};

struct Interface {
    GString name;
    GMap<GString, std::pair<Procedure*, size_t>> methods;
    size_t _typeid;
    
};

namespace std {
template <class T>
inline void hash_combine(std::size_t& seed, const T& v) {
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

template<> struct hash<DataType> {
    size_t operator()(const DataType& dt) const noexcept {
        size_t seed = 0;
        if (dt.is_object()) 
            hash_combine(seed, dt.getobjectname());
        else 
            hash_combine(seed, static_cast<int>(dt.root().getsimpletype()));

        hash_combine(seed, dt.root().ptrlvl);
        
        for (const auto& g : dt.node->generics) {
            hash_combine(seed, hash<DataType>{}(g));
        }
        return seed;
    }
};

template<> struct hash<GVector<DataType>> {
    size_t operator()(const GVector<DataType>& type_vector) const noexcept {
        size_t seed = 0;

        for (const auto& t : type_vector) {
            hash_combine(seed, hash<DataType>{}(t));
        }

        return seed;
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
    GMap<NodeTermCall*, std::variant<Procedure*, Struct*>> m_mapped_calls_symbols;
    GMap<NodeTermNmCall*, Procedure*> m_mapped_nm_calls_symbols;
    GMap<NodeStmtLet*, TermIdentSymbol> m_mapped_let_symbols;
    GMap<NodeTermNmIdent*, GlobalVariable*> m_mapped_nmident_symbols;

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
    NodeStmtWhile* m_cur_while = nullptr;

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
                                    GVector<DataType> args, GVector<DataType> template_args_explicit)
    {
        GVector<Procedure*> candidates { procedure };
        candidates.insert(candidates.end(), procedure->overloads.begin(), procedure->overloads.end());
        GVector<std::pair<Procedure*, GMap<GString, DataType>>> ideals {};
        for (Procedure* cand : candidates) {
            if (cand->params.size() != args.size()) continue;
            bool is_template = (cand->templates != nullptr && !cand->templates->empty());
            
            if (is_template && !template_args_explicit.empty()) {
                if (template_args_explicit.size() != cand->templates->size()) continue;
            }
            GVector<std::pair<GString, DataType>> check_params = cand->params;
            GMap<GString, DataType> deduced_map;
            if (is_template) {
                if (!template_args_explicit.empty()) {
                    for (size_t i = 0; i < template_args_explicit.size(); ++i) {
                        deduced_map[(*cand->templates)[i]] = template_args_explicit[i];
                    }
                } else {
                    deduced_map = template_deduction(args, cand->params, cand->templates, def, cand);
                    if (deduced_map.empty()) continue;
                }
                apply_template_substitution(check_params, deduced_map);
            }
            std::pair<bool, size_t> match = match_call_signature(args, check_params, cand);
            if (match.first) {   
                if (is_template) {
                    ideals.push_back({cand, deduced_map});
                } else {
                    return cand;
                }
            }
        }
        while(!ideals.empty()) {
            auto current = ideals.back();
            Procedure* cand = current.first;
            ideals.pop_back();
            if(cand->params.size() == args.size()) {
                GVector<DataType> final_targs;

                GMap<GString, DataType>& deduced_map = current.second;
                
                for(const GString& tname : *cand->templates) {
                    final_targs.push_back(deduced_map[tname]);
                }
                return m_template_instantiator.instantiate_procedure(cand, final_targs, def);
            }
        }
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
    
    void analyze_scope_with_begin_scope(NodeScope* scope) {
        m_sym_table.begin_scope();
        analyze_scope(scope);
        m_sym_table.end_scope();
    }

    bool deduce_recursive(const DataType& formal, const DataType& actual, 
                          const GVector<GString>* templates, 
                          GMap<GString, DataType>& deduced) 
    {
        GString formal_name;
        if (formal.is_object()) {
            formal_name = formal.getobjectname();
        } else if (formal.is_simple()) {
             formal_name = formal.to_string();
        }

        bool is_template_param = false;
        if (!formal_name.empty()) {
            for (const GString& t_name : *templates) {
                if (formal_name == t_name) {
                    is_template_param = true;
                    break;
                }
            }
        }
        if (is_template_param) {
            if (formal.root().ptrlvl > actual.root().ptrlvl) return false;
    
            DataType deduced_type(actual.root());
            deduced_type.node->generics = actual.node->generics;
    
            deduced_type.root().ptrlvl -= formal.root().ptrlvl;
            
            const auto& it = deduced.find(formal_name);
            if (it != deduced.end()) {
                if (!types_equ(it->second, deduced_type)) return false;
            } else {
                deduced[formal_name] = deduced_type;
            }
            return true;
        }

        if (formal.root().ptrlvl != actual.root().ptrlvl) return false;
        
        if (formal.is_object() && actual.is_object()) {
            if (formal.getobjectname() != actual.getobjectname()) return false;

            const auto& f_gens = formal.node->generics;
            const auto& a_gens = actual.node->generics;

            if (f_gens.size() != a_gens.size()) return false;

            for (size_t i = 0; i < f_gens.size(); ++i) {
                if (!deduce_recursive(f_gens[i], a_gens[i], templates, deduced)) return false;
            }
            return true;
        }
        
        return types_equ(formal, actual);
    }

    GMap<GString, DataType> template_deduction(
        const GVector<DataType>& args_types, 
        const GVector<std::pair<GString, DataType>>& params, 
        const GVector<GString>* templates, const Token& def, const Procedure* proc) 
    {
        GMap<GString, DataType> deduced_map;

        if (templates == nullptr || templates->empty()) return deduced_map;

        size_t count = std::min(args_types.size(), params.size());

        for (size_t i = 0; i < count; ++i) {
            const DataType& actual = args_types[i];
            const DataType& formal = params[i].second;

            if (!deduce_recursive(formal, actual, templates, deduced_map)) {
                m_diag_man->DiagnosticMessage(def, "error", "template deduction failed for argument " + std::to_string(i+1), 0);
                exit(EXIT_FAILURE);
            }
        }

        for (const GString& t_name : *templates) {
            if (deduced_map.find(t_name) == deduced_map.end()) {
                m_diag_man->DiagnosticMessage(def, "error", "could not deduce template parameter `" + t_name + "`", 0);
                if(proc != nullptr) m_diag_man->DiagnosticMessage(proc->from->def, "note", "template procedure defined here", 0);
                exit(EXIT_FAILURE);
            }
        }

        return deduced_map;
    }

    void apply_template_substitution(
        GVector<std::pair<GString, DataType>>& params, 
        const GMap<GString, DataType>& temps)          
    {
        for (auto& param : params) {
            std::function<void(DataType&)> sub = [&](DataType& type) {
                if (type.is_object()) {
                    auto it = temps.find(type.getobjectname());
                    if (it != temps.end()) {
                        DataType new_type = it->second;
                        if(type.root().ptrlvl > 0) new_type.root().ptrlvl += type.root().ptrlvl;
                        type = new_type;
                        return;
                    }
                }
                for (auto& g : type.node->generics) {
                    sub(g);
                }
            };
            
            sub(param.second);
        }
    }

    Procedure* resolve_call(Procedure* proc, const GVector<NodeExpr*>& args, GVector<DataType>& targs, const Token& def) {
        GVector<DataType> args_types;
        for(size_t i = 0;i < args.size();i++) {
            args_types.push_back(analyze_expr(args[i]));
        }

        if (proc->templates != NULL && targs.empty() && proc->overloads.empty()) {
            GMap<GString, DataType> deduced = template_deduction(args_types, proc->params, proc->templates, def, proc);
            
            if (!deduced.empty()) {
                for (const GString& t_name : *proc->templates) {
                    targs.push_back(deduced[t_name]);
                }
            }
        }

        if(proc->templates != NULL && proc->overloads.empty()) {
            proc = m_template_instantiator.instantiate_procedure(proc, targs, def);
        }

        if(!proc->overloads.empty()) {
            proc = resolve_overloading(def, proc, args_types, targs);
        } else {
            std::pair<bool, size_t> typecheck_result = match_call_signature(args_types, proc->params, proc);
            if(!typecheck_result.first) {
                m_diag_man->DiagnosticMessage(proc->def, "error", "procedure `" + proc->name + "` expects an `" + proc->params[typecheck_result.second].second.to_string() + "` " + GString(std::to_string(typecheck_result.second + 1).c_str()) + " argument type.", 0);
                m_diag_man->DiagnosticMessage(def, "note", "but got `" + args_types[typecheck_result.second].to_string() + "`", 0);
                exit(EXIT_FAILURE);
            }
        }
        return proc;
    }

    void specialize_structure_with_templates(const GVector<DataType>& template_args, Struct* structure) {
        const auto& search = structure->specializations.find(template_args);
        if(search != structure->specializations.end()) return;

        Struct* new_speacialization = m_allocator->emplace<Struct>();
        new_speacialization->name = structure->name;
        new_speacialization->temps = {};
        new_speacialization->def = structure->def;
        new_speacialization->temp = false;
        new_speacialization->parent_type = structure->parent_type;
        new_speacialization->specializations = {};

        GMap<GString, DataType> mapped_templates;
        for(size_t i = 0;i < template_args.size(); ++i) {
            mapped_templates[structure->temps[i]] = template_args[i];
        }

        new_speacialization->__fields = structure->__fields;
        apply_template_substitution(new_speacialization->__fields, mapped_templates);
        new_speacialization->fields = compute_fields(new_speacialization->__fields);

        structure->specializations[template_args] = new_speacialization;
    }

    DataType analyze_object_creation(NodeTermCall* term_call, Struct* structure) {
        if(structure->temp && structure->temps.size() != term_call->targs.size()) {
            m_diag_man->DiagnosticMessage(term_call->def, "error", "structure `" + structure->name + "` excepts " + GString(std::to_string(structure->temps.size()).c_str()) + " template arguments, but got " + GString(std::to_string(term_call->targs.size())) + ".", 0);
            exit(EXIT_FAILURE);
        }
        DataType result_type(BaseDataType(structure->name));

        if(structure->temp) {
            size_t i = 0ULL;
            for([[maybe_unused]] auto&& temp : structure->temps) {
                result_type.node->generics.push_back(term_call->targs[i++]);
            }
            specialize_structure_with_templates(term_call->targs, structure);
        }
        GVector<NodeExpr*> args;

        if(term_call->args.has_value()) {
            args = __getargs(term_call->args.value());
        }

        if(args.size() != 0ULL && args.size() != structure->fields.size()) {
            m_diag_man->DiagnosticMessage(term_call->def, "error", "incorrect number of arguments for initializing an object of type `" + result_type.to_string() + "`, expected " + GString(std::to_string(structure->fields.size()).c_str()) + ", received " + GString(std::to_string(args.size()).c_str()) + ".", 0);
            exit(EXIT_FAILURE);
        }

        return result_type;
    }

    DataType analyze_field_access(NodeBinExprDot* dot, DataType object_type,
                                NodeTermIdent* ident, const Token& def)
    {
        const GString& field_name = ident->ident.value.value();

        if(!object_type.is_object()) {
            m_diag_man->DiagnosticMessage(def, "error", "you cannot get the `" + field_name + "` field from a non-object type `" + object_type.to_string() + "`.", 0);
            exit(EXIT_FAILURE);
        }

        const GString& object_name = object_type.getobjectname();

        std::optional<Struct*> maybe_existing_structure = m_sym_table.struct_lookup(object_name);

        assert(maybe_existing_structure.has_value());

        Struct* existing_structure = maybe_existing_structure.value();

        if(existing_structure->temp) {
            const auto& specialization_search = existing_structure->specializations.find(object_type.node->generics);
            assert(specialization_search != existing_structure->specializations.end());
            existing_structure = specialization_search->second;
            assert(existing_structure != nullptr);
        }

        const auto& search = existing_structure->fields.find(field_name);
        if(search == existing_structure->fields.end()) {
            m_diag_man->DiagnosticMessage(def, "error", "a `" + object_name + "` object does not have a field `" + field_name + "`.", 0);
            exit(EXIT_FAILURE);
        }

        dot->resolved_field = &(search->second);

        return search->second.type;
    }

    void define_namespace_and_analyze_scope(const GString& name, NodeScope* scope, SemanticScope* where) {
        Namespace* old_namespace = m_cur_namespace;

        Namespace* current_namespace = nullptr;

        auto existing = m_sym_table.namespace_lookup(name);
        if(!existing.has_value()) {
            current_namespace = m_allocator->emplace<Namespace>();
            current_namespace->parent = old_namespace;
            where->m_namespaces[name] = current_namespace;
            current_namespace->name = name;
            current_namespace->scope = m_allocator->emplace<SemanticScope>();
        } else {
            current_namespace = existing.value();
        }

        m_cur_namespace = current_namespace;
        m_sym_table.m_scopes.push_front(current_namespace->scope); // begin_scope()

        analyze_scope(scope);

        m_sym_table.end_scope();

        m_cur_namespace = old_namespace;
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
                return BaseDataTypeInt;
            }

            DataType operator()([[maybe_unused]] const NodeTermSizeof* term_sizeof) const {
                return BaseDataTypeVoid;
            }

            DataType operator()(const NodeTermRd* term_rd) const {
                sema.analyze_expr(term_rd->expr);
                return BaseDataTypeInt;
            }

            DataType operator()(const NodeTermCast* term_cast) const {
                sema.check_type_is_valid(term_cast->def, term_cast->type);
                return term_cast->type;
            }

            DataType operator()(const NodeTermUnref* term_unref) const {
                DataType type = sema.analyze_expr(term_unref->expr);
                if (type.root().ptrlvl == 0) {
                     sema.m_diag_man->DiagnosticMessage(
                         Token{}, 
                         "error", 
                         "cannot dereference a non-pointer type `" + type.to_string() + "`", 
                         0
                     );
                     exit(EXIT_FAILURE);
                }

                DataType dereferenced_type(type.root());
                dereferenced_type.node->generics = type.node->generics;

                dereferenced_type.root().ptrlvl -= 1;

                return dereferenced_type;
            }

            DataType operator()(const NodeTermCastTo* term_cast_to) const {
                sema.analyze_expr(term_cast_to->expr);
                return sema.analyze_expr(term_cast_to->to);
            }

            DataType operator()([[maybe_unused]] const NodeTermTypeid* term_typeid) const {
                return BaseDataTypeInt;
            }

            DataType operator()([[maybe_unused]] const NodeTermStrLit* term_str_lit) const {
                DataType dt(BaseDataTypeChar);
                dt.root().ptrlvl++;
                return dt;
            }

            DataType operator()([[maybe_unused]] const NodeTermAmpersand* term_amp) const {
                DataType dt = sema.analyze_expr(term_amp->expr, lvalue);
    
                DataType ptr_type(dt.root());
                ptr_type.node->generics = dt.node->generics;
                
                ptr_type.root().ptrlvl += 1;
                return ptr_type;
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

            DataType operator()(const NodeTermParen* term_paren) const {
                return sema.analyze_expr(term_paren->expr);   
            }

            DataType operator()(NodeTermNmCall* term_call) const {
                assert(term_call->nm.size() > 0);

                std::optional<Namespace*> _namesp = sema.m_sym_table.namespace_lookup(term_call->nm[0]);
                if(!_namesp.has_value()) {
                    sema.m_diag_man->DiagnosticMessage(term_call->def, "error", "unkown namespace `" + term_call->nm[0] + "`", 0);
                    exit(EXIT_FAILURE);
                }

                Namespace* current_nm = _namesp.value();
                for(size_t i = 1;i < term_call->nm.size();i++) {
                    std::optional<Namespace*> _nm = current_nm->scope->namespace_lookup(term_call->nm[i]);
                    if(!_nm.has_value()) {
                        GString errloc_namespace = term_call->nm[0];
                        if(term_call->nm.size() > 1) errloc_namespace += "::";
                        for(size_t j = 1;j < term_call->nm.size();j++) {
                            errloc_namespace += term_call->nm[j];
                            if(j != term_call->nm.size() - 1) {
                                errloc_namespace += "::";
                            }
                        }
                        sema.m_diag_man->DiagnosticMessage(term_call->def, "error", "unkown namespace `" + errloc_namespace + "`", 0);
                        exit(EXIT_FAILURE);
                    }
                    current_nm = _nm.value();
                }

                assert(current_nm != nullptr);

                const GString& name = term_call->name;

                std::optional<Procedure*> _proc = current_nm->scope->proc_lookup(name);

                if(!_proc.has_value()) {
                    const GString& path_to_nm = current_nm->get_path();
                    sema.m_diag_man->DiagnosticMessage(term_call->def, "error", "namespace `" + path_to_nm.substr(0, path_to_nm.length() - 2) + "` doesn't have procedure `" + name + "`", 0);
                    exit(EXIT_FAILURE);
                }

                Procedure* proc = _proc.value();

                GVector<NodeExpr*> args{};

                if(term_call->args.has_value()) {
                    args = sema.__getargs(term_call->args.value());
                }

                proc = sema.resolve_call(proc, args, term_call->targs, term_call->def);

                sema.m_sym_table.m_mapped_nm_calls_symbols[term_call] = proc;

                return proc->rettype;
            }

            DataType operator()(NodeTermCall* term_call) const {
                const GString& name = term_call->name;
                std::optional<Procedure*> _procedure = sema.m_sym_table.proc_lookup(name);

                if(!_procedure.has_value()) {

                    std::optional<Struct*> maybe_existing_structure = sema.m_sym_table.struct_lookup(name);
                    if(maybe_existing_structure.has_value()) {
                        sema.m_sym_table.m_mapped_calls_symbols[term_call] = maybe_existing_structure.value();
                        return sema.analyze_object_creation(term_call, maybe_existing_structure.value());
                    }

                    sema.m_diag_man->DiagnosticMessage(term_call->def, "error", "undefined procedure `" + name + "`", 0);
                    exit(EXIT_FAILURE);
                }

                Procedure* proc = _procedure.value();

                GVector<NodeExpr*> args{};

                if(term_call->args.has_value()) {
                    args = sema.__getargs(term_call->args.value());
                }

                proc = sema.resolve_call(proc, args, term_call->targs, term_call->def);

                sema.m_sym_table.m_mapped_calls_symbols[term_call] = proc;

                return proc->rettype;
            }

            DataType operator()(const NodeTermMtCall* term_call) const {
                DataType object_type = sema.analyze_expr(term_call->mt);
                assert(object_type.is_object());

                NodeTermNmCall* nm_call = sema.m_allocator->emplace<NodeTermNmCall>();
                nm_call->def = term_call->def;
                nm_call->name = term_call->name;
                nm_call->nm = { object_type.getobjectname() };
                nm_call->args = term_call->args;
                nm_call->targs = term_call->targs;
                nm_call->as_expr = true;

                base_expr->var = sema.as_expr_pointer(sema.as_term_pointer(nm_call))->var;
                return sema.analyze_expr(base_expr);
            }
            DataType operator()(NodeTermNmIdent* nm_ident) const {
                assert(nm_ident->nm.size() > 0);

                std::optional<Namespace*> _namesp = sema.m_sym_table.namespace_lookup(nm_ident->nm[0]);
                if(!_namesp.has_value()) {
                    sema.m_diag_man->DiagnosticMessage(nm_ident->def, "error", "unkown namespace `" + nm_ident->nm[0] + "`", 0);
                    exit(EXIT_FAILURE);
                }

                Namespace* current_nm = _namesp.value();
                for(size_t i = 1;i < nm_ident->nm.size();i++) {
                    std::optional<Namespace*> _nm = current_nm->scope->namespace_lookup(nm_ident->nm[i]);
                    if(!_nm.has_value()) {
                        GString errloc_namespace = nm_ident->nm[0];
                        if(nm_ident->nm.size() > 1) errloc_namespace += "::";
                        for(size_t j = 1;j < nm_ident->nm.size();j++) {
                            errloc_namespace += nm_ident->nm[j];
                            if(j != nm_ident->nm.size() - 1) {
                                errloc_namespace += "::";
                            }
                        }
                        sema.m_diag_man->DiagnosticMessage(nm_ident->def, "error", "unkown namespace `" + errloc_namespace + "`", 0);
                        exit(EXIT_FAILURE);
                    }
                    current_nm = _nm.value();
                }

                const GString& variable_name = nm_ident->name;

                std::optional<GlobalVariable*> maybe_existing_global_variable = current_nm->scope->gvar_lookup(variable_name);
                if(!maybe_existing_global_variable.has_value()) {
                    sema.m_diag_man->DiagnosticMessage(nm_ident->def, "error", "could not find the symbol `" + current_nm->get_path() + variable_name + "`.", 0);
                }

                GlobalVariable* var = maybe_existing_global_variable.value();

                sema.m_sym_table.m_mapped_nmident_symbols[nm_ident] = var;

                return var->type;


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

            DataType operator()(NodeBinExprDot* dot) const {
                DataType object_type = sema.analyze_expr(dot->lhs);

                if(std::holds_alternative<NodeTerm*>(dot->rhs->var)) {

                    NodeTerm* rhs_as_term = std::get<NodeTerm*>(dot->rhs->var);

                    if(std::holds_alternative<NodeTermIdent*>(rhs_as_term->var)) {
                        return sema.analyze_field_access(dot, object_type, std::get<NodeTermIdent*>(rhs_as_term->var), base->def);
                    }

                    if(std::holds_alternative<NodeTermCall*>(rhs_as_term->var)) {
                        NodeTermCall* as_term_call = std::get<NodeTermCall*>(rhs_as_term->var);

                        GVector<NodeExpr*> args;
                        if(as_term_call->args.has_value()) args = sema.__getargs(as_term_call->args.value());
                        args.insert(args.begin(), dot->lhs);

                        NodeExpr* call_expr = sema.construct_method_call(base->def, dot->lhs, as_term_call->name, 
                                                                    args, as_term_call->targs);
                        base_expr->var = call_expr->var;
                        return sema.analyze_expr(base_expr);
                    }
                }

                sema.m_diag_man->DiagnosticMessage(base->def, "error", "after . excepted field name or a method call.", 0);
                exit(EXIT_FAILURE);
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

            void operator()(const NodeIfPredElif* elif) const
            {
                sema.analyze_expr(elif->expr);
                sema.analyze_scope_with_begin_scope(elif->scope);
                if(elif->pred.has_value()) sema.analyze_if_pred(elif->pred.value());
            }

            void operator()(const NodeIfPredElse* else_) const
            {
                sema.analyze_scope_with_begin_scope(else_->scope);
            }
        };

        PredVisitor visitor{ *this };
        std::visit(visitor, pred->var);
    }

    void analyze_scope(NodeScope* scope) {
        size_t saved_stack_align = 0;
        if (m_cur_procedure) {
            saved_stack_align = m_cur_procedure->stack_allign;
        }

        for(NodeStmt* stmt : scope->stmts) {
            analyze_stmt(stmt);
        }

        if (m_cur_procedure) {
            if(m_cur_procedure->stack_allign > m_cur_procedure->max_stack_allign) m_cur_procedure->max_stack_allign = m_cur_procedure->stack_allign;
            m_cur_procedure->stack_allign = saved_stack_align;
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

        bool cimport = std::find(stmt_proc->attrs.begin(), stmt_proc->attrs.end(), ProcAttr::cimport) != stmt_proc->attrs.end();

        const auto& link_decorator_search = stmt_proc->decorators.find("link_name");

        if(link_decorator_search == stmt_proc->decorators.end()) {
            if(cimport) {
                cproc->mangled_symbol = stmt_proc->name;
            }
            else {
                GString mangled_symbol = stmt_proc->name;
                for(size_t i = 0ULL;i < stmt_proc->params.size();i += 1) {
                    mangled_symbol += stmt_proc->params[i].second.sign();
                }
                mangled_symbol += "@" + std::to_string(stmt_proc->params.size());
                cproc->mangled_symbol = mangled_symbol;
            }
        } else {
            cproc->mangled_symbol = link_decorator_search->second;
            if(stmt_proc->templates != NULL) {
                m_diag_man->DiagnosticMessage(stmt_proc->def, "error", "a template procedure cannot have a specified linking name", 0);
                exit(EXIT_FAILURE);
            }
        }

        cproc->nmspace = m_cur_namespace;
        return cproc;
    }

    bool procedure_in_namespace_need_mangle(Procedure* procedure) {
        return procedure->nmspace != nullptr &&
                std::find(procedure->attrs.begin(), procedure->attrs.end(), ProcAttr::cimport) == procedure->attrs.end() &&
                procedure->from->decorators.find("link_name") == procedure->from->decorators.end();
    }

    void analyze_procedure(Procedure* procedure) {
        if(procedure_in_namespace_need_mangle(procedure)) {
            procedure->mangled_symbol = procedure->nmspace->get_mangle() + procedure->mangled_symbol;
            procedure->name = procedure->nmspace->get_path() + procedure->from->name;
        }

        if(procedure->from->prototype || procedure->from->templates != nullptr) return;
        // TODO: When you call analyze_procedure from analyze_prog
        // the m_cur_namespace variable is likely nullptr
        // if a template was defined within namespace A
        // and a global variable from A (without the A:: prefix) is used within the template
        // the analyzer may not find it because it doesn't know what's "inside" namespace A.
        m_cur_procedure = procedure;
        m_sym_table.begin_scope();
        for(size_t i = 0;i < procedure->params.size();i++) {
            define_local_variable(procedure->params[i].first, procedure->params[i].second);
        }
        analyze_scope(procedure->from->scope);
        m_sym_table.end_scope();
        m_cur_procedure = nullptr;
    }

    GVector<DataType> get_template_args(const DataType& dt) {
        return dt.node->generics;
    }

    using mapped_temps = GMap<GString, DataType>;

    mapped_temps compute_temps(const GVector<GString>& templates, const GVector<DataType>& targs) {
        assert(templates.size() == targs.size());
        mapped_temps temps;
        size_t counter{ 0 };
        for (auto&& el : templates) {
            temps[el] = targs[counter++];
        }
        return temps;
    }

    GMap<GString, Field> compute_fields(const GVector<std::pair<GString, DataType>>& fields) {
        GMap<GString, Field> __fields;
        size_t nth = 0ULL;
        for (const std::pair<GString, DataType>& field : fields) {
            __fields[field.first] = { .name = field.first, .type = field.second, .nth = nth++ };
        }
        return __fields;
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
                
                if(explicit_type.has_value() && stmt_let->expr.has_value()) {
                    const DataType& explicit_type_unpacked = explicit_type.value();
                    if(!sema.types_equ(explicit_type_unpacked, expression_type)) {
                        sema.m_diag_man->DiagnosticMessage(stmt_let->ident, "error", "when defining the variable, the expected type was `" + explicit_type_unpacked.to_string() + "`, but the type `" + expression_type.to_string() + "` was obtained.", name.length() + 2);
                        exit(EXIT_FAILURE);
                    }
                }

                if(sema.m_sym_table.m_scopes.size() == 1) {
                    GlobalVariable* to_insert = sema.m_allocator->emplace<GlobalVariable>();
                    to_insert->name = name;
                    to_insert->type = expression_type;
                    to_insert->mangled_symbol = "_v_" + name;
                    sema.m_sym_table.m_gvars[name] = to_insert;

                    sema.m_sym_table.m_mapped_let_symbols[stmt_let] = TermIdentSymbol {
                        TermIdentSymbolKind::GLOBAL_VAR, reinterpret_cast<void*>(to_insert)
                    };
                } else {
                    if(sema.m_cur_namespace != nullptr && sema.m_cur_procedure == nullptr) {
                        GlobalVariable* to_insert = sema.m_allocator->emplace<GlobalVariable>();
                        to_insert->name = name;
                        to_insert->type = expression_type;
                        to_insert->mangled_symbol = "_v@" + sema.m_cur_namespace->get_mangle() + name;
                        sema.m_cur_namespace->scope->m_gvars[name] = to_insert;
                        sema.m_sym_table.m_gvars[to_insert->mangled_symbol] = to_insert;
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

            void operator()(const NodeStmtAssign* stmt_assign) const
            {
                DataType lvalue_type = sema.analyze_expr(stmt_assign->lvalue, true);
                DataType rvalue_type = sema.analyze_expr(stmt_assign->expr);
                if(lvalue_type != rvalue_type) {
                    sema.m_diag_man->DiagnosticMessage(stmt_assign->def, "error", "the assignment expected an type `" + lvalue_type.to_string() + "`, but got a type `" + rvalue_type.to_string() + "`", 0);
                    exit(EXIT_FAILURE);
                }
            }

            void operator()([[maybe_unused]] const NodeStmtIncBy* stmt_assign) const
            {
                DataType lvalue_type = sema.analyze_expr(stmt_assign->lvalue, true);
                DataType rvalue_type = sema.analyze_expr(stmt_assign->expr);
                if(lvalue_type != rvalue_type) {
                    sema.m_diag_man->DiagnosticMessage(stmt_assign->def, "error", "the increment expected an type `" + lvalue_type.to_string() + "`, but got a type `" + rvalue_type.to_string() + "`", 0);
                    exit(EXIT_FAILURE);
                }
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

            void operator()(const NodeStmtPushOnStack* stmt_push) const
            {
                sema.analyze_expr(stmt_push->expr);
            }

            void operator()(const NodeStmtIf* stmt_if) const
            {
                sema.analyze_expr(stmt_if->expr);
                sema.m_sym_table.begin_scope();
                sema.analyze_scope(stmt_if->scope);
                sema.m_sym_table.end_scope();
                if(stmt_if->pred.has_value()) sema.analyze_if_pred(stmt_if->pred.value());
            }

            void operator()(NodeStmtWhile* stmt_while) const
            {
                sema.analyze_expr(stmt_while->expr);
                NodeStmtWhile* old_while = sema.m_cur_while;
                sema.m_cur_while = stmt_while;
                sema.analyze_scope(stmt_while->scope);
                sema.m_cur_while = old_while;
            }

            void operator()(NodeStmtBreak* stmt_break) const
            {
                if(sema.m_cur_while == nullptr) {
                    sema.m_diag_man->DiagnosticMessage(stmt_break->def, "error", "break without while", 0);
                    exit(EXIT_FAILURE);
                }
                stmt_break->from = sema.m_cur_while;
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

            void operator()(NodeStmtStruct* stmt_struct) const
            {
                std::optional<Struct*> already_declared = sema.m_sym_table.struct_lookup(stmt_struct->name);
                if(already_declared.has_value()) {
                    sema.m_diag_man->DiagnosticMessage(stmt_struct->def, "error", "redefenition of structure `" + stmt_struct->name + "`", 0);
                    sema.m_diag_man->DiagnosticMessage(already_declared.value()->def, "note", "first defenition here", 0);
                    exit(EXIT_FAILURE);
                }
                GVector<std::pair<GString, DataType>> final_fields;
                std::optional<DataType> parent_dt = stmt_struct->parent;

                if (parent_dt.has_value()) {
                    GString pname = parent_dt.value().getobjectname();
                    std::optional<Struct*> p_struct_opt = sema.m_sym_table.struct_lookup(pname);
                    
                    if (!p_struct_opt.has_value()) {
                        sema.m_diag_man->DiagnosticMessage(stmt_struct->def, "error", "parent struct `" + pname + "` not found", 0);
                        exit(EXIT_FAILURE);
                    }
                    
                    Struct* p_struct = p_struct_opt.value();
                    
                    final_fields = p_struct->__fields;
                    
                    if (p_struct->temp) {
                        GVector<DataType> targs = sema.get_template_args(parent_dt.value());
                        
                        if (targs.size() != p_struct->temps.size()) {
                            sema.m_diag_man->DiagnosticMessage(stmt_struct->def, "error", "parent struct template args mismatch", 0);
                            exit(EXIT_FAILURE);
                        }

                        GMap<GString, DataType> temps = sema.compute_temps(p_struct->temps, targs);

                        TypeSubstitutor substitutor(temps);
                        
                        for(auto& f : final_fields) {
                            f.second = substitutor.substitute_type(f.second);
                        }
                    }
                }
                
                final_fields.insert(final_fields.end(), stmt_struct->fields.begin(), stmt_struct->fields.end());

                Struct* to_insert = sema.m_allocator->emplace<Struct>();
                to_insert->name = stmt_struct->name;
                to_insert->fields = sema.compute_fields(final_fields);
                to_insert->temps = stmt_struct->temps;
                to_insert->temp = stmt_struct->temp;
                to_insert->def = stmt_struct->def;
                to_insert->__fields = final_fields;
                to_insert->parent_type = stmt_struct->parent;

                sema.m_sym_table.last_scope()->m_structs[stmt_struct->name] = to_insert;
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
                sema.define_namespace_and_analyze_scope(stmt_space->name, stmt_space->scope, sema.m_sym_table.last_scope());
            }

            void operator()(const NodeStmtImpl* stmt_impl) const
            {
                const GString& implementation_name = stmt_impl->name;

                std::optional<Struct*> maybe_existing_structure = sema.m_sym_table.struct_lookup(implementation_name);
                if(!maybe_existing_structure.has_value()) {
                    sema.m_diag_man->DiagnosticMessage(stmt_impl->def, "error", "undefined structure `" + implementation_name + "`.", 0);
                    exit(EXIT_FAILURE);
                }

                if(!stmt_impl->temps.empty()) {
                    for(NodeStmt* stmt : stmt_impl->scope->stmts) {
                        if(std::holds_alternative<NodeStmtProc*>(stmt->var)) {
                            NodeStmtProc* procedure_definition = std::get<NodeStmtProc*>(stmt->var);
                            const GString& procedure_name = procedure_definition->name;
                            if(stmt_impl->inst.find(procedure_name) == stmt_impl->inst.end()) {
                                if(procedure_definition->templates == NULL) {
                                    procedure_definition->templates = sema.m_allocator->emplace<GVector<GString>>();
                                }
                                procedure_definition->templates->insert(procedure_definition->templates->begin(),
                                                                stmt_impl->temps.begin(), stmt_impl->temps.end());
                            }
                        }
                    }
                }

                sema.define_namespace_and_analyze_scope(implementation_name, stmt_impl->scope, sema.m_sym_table.last_scope());
            }

            void operator()(NodeStmtNmCall* stmt_call) const
            {
                NodeTermNmCall* call = sema.m_allocator->emplace<NodeTermNmCall>();
                call->def = stmt_call->def;
                call->nm = stmt_call->nm;
                call->name = stmt_call->name;
                call->args = stmt_call->args;
                call->targs = stmt_call->targs;
                call->as_expr = false;
                stmt_call->resolved_expression = sema.as_expr_pointer(sema.as_term_pointer(call));
                sema.analyze_expr(stmt_call->resolved_expression);
            }

            void operator()(const NodeStmtMtCall* stmt_call) const
            {
                DataType object_type = sema.analyze_expr(stmt_call->mt);
                assert(object_type.is_object());

                NodeStmtNmCall* nm_call = sema.m_allocator->emplace<NodeStmtNmCall>();
                nm_call->def = stmt_call->def;
                nm_call->name = stmt_call->name;
                nm_call->nm = { object_type.getobjectname() };
                nm_call->args = stmt_call->args;
                nm_call->targs = stmt_call->targs;

                base_stmt->var = nm_call;
                sema.analyze_stmt(base_stmt);  
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
                template_map[template_procedure->templates->operator[](i)] = template_args[i];
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