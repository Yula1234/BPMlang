#pragma once

#define VectorSimDataCap 4096

#define TYPEID_INT  0
#define TYPEID_PTR  1
#define TYPEID_VOID 2
#define TYPEID_ANY  3
#define TYPEID_CHAR 4

#define __DTOR_PREFIX "__dtor__"

#define v_alt(__v, __tp) std::holds_alternative<__tp>(__v)
#define v_get(__v, __tp) std::get<__tp>(__v)

#define UNUSED_ARG __attribute__((unused))

using __str_ref = const GString&;

void consume_un(...) {}

namespace INTERNAL_CODE {
    GString IMPLEMENTATION = R"(struct exception { __message: char*, __bstub1: int, __bstub2: int }
impl exception { proc what(exception self) -> char* { return self.__message; } }
struct __DoubleFreeException { __addr: ptr, __bstub1: int, __bstub2: int }
impl __DoubleFreeException { proc what(__DoubleFreeException self) -> char* {
        __pushonstack(self.__addr);
        asm "call __bpm_double_free_exception_what";
        asm "add esp, 4";
        asm "push eax";
        let __fst = __popfromstack();
        return cast(char*, __fst); } }
__oninit { __pushonstack(typeid(__DoubleFreeException)); asm "pop edx"; asm "mov dword [__BpmDoubleExceptionTypeId], edx"; }
struct __RecursionException { __bstub: int, __bstub1: int, __bstub2: int }
impl __RecursionException { proc what(__RecursionException self) -> char* {
        asm "call __bpm_recursion_exception_what";
        asm "push eax";
        let __fst = __popfromstack();
        return cast(char*, __fst); } }
__oninit { __pushonstack(typeid(__RecursionException)); asm "pop edx"; asm "mov dword [__BpmRecursionExceptionTypeId], edx"; }
struct __SigSegvException { __addr: int, __bstub1: int, __bstub2: int }
impl __SigSegvException { proc what(__SigSegvException self) -> char* {
        __pushonstack(self.__addr);
        asm "call __sigsegv_wh_exception";
        asm "add esp, 4";
        asm "push eax";
        let __fst = __popfromstack();
        return cast(char*, __fst); } }
__oninit { __pushonstack(typeid(__SigSegvException)); asm "pop edx"; asm "mov dword [__BpmSigSegvExceptionTypeId], edx"; }
namespace std { proc exception(char* mess_) -> exception = return exception(mess_, 0, 0); }
interface __ObjectTypeI {}
interface __SimpleTypeI {}
interface __PointerTypeI {}
interface __StdAddable<T> { proc m_add(self, T other) -> T; }
interface __StdSubstractable<T> { proc m_sub(self, T other) -> T; }
interface __StdMultipliable<T> { proc m_mul(self, T other) -> T; }
interface __StdDivisible<T> { proc m_div(self, T other) -> T; }
interface __StdEquatable<T> { proc m_equal(self, T other) -> int; }
interface __StdAssignable<T> { proc m_assign(self, T other) -> void; }
proc memcpy(ptr __src, ptr __dst, int __size) -> void;)";
}

template<typename T>
class VectorSim {
private:
    T      m_data[VectorSimDataCap];
    size_t m_size = 0ULL;
public:
    inline size_t size() const noexcept { return m_size; }
    inline void   pop_back() noexcept   { m_size--; }
    inline T&     operator[](const size_t _A_index) noexcept { return m_data[_A_index]; }
    inline void   push_back(const T& _A_element) noexcept    { m_data[m_size++] = _A_element; }
};

namespace std {
template<>
struct hash<std::pair<GString, GString>> {
    size_t operator()(const std::pair<GString, GString>& p) const noexcept {
        std::hash<GString> h;
        size_t seed = h(p.first);
        size_t h2   = h(p.second);
        seed ^= h2 + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
        return seed;
    }
};
}

class Generator {
public:
    inline Operand reg(Reg r) const              { return Operand::regOp(r); }
    inline Operand imm(int32_t v) const          { return Operand::immOp(v); }
    inline Operand label(const GString& s) const { return Operand::labelOp(s); }
    inline Operand sym(const GString& s) const   { return Operand::symbolOp(s); }
    inline Operand mem(const MemRef& m) const    { return Operand::memOp(m); }

    inline MemRef local_mem(size_t stack_loc) const {
        return MemRef::baseDisp(Reg::EBP, -static_cast<int32_t>(stack_loc));
    }
    inline MemRef global_mem(const GString& name) const {
        return MemRef::sym("v_" + name);
    }

    inline void push_reg(Reg r)                { m_builder.push(reg(r)); }
    inline void push_imm(int32_t v)            { m_builder.push(imm(v)); }
    inline void push_mem(const MemRef& m)      { m_builder.push(mem(m)); }
    inline void push_sym(const GString& s) { m_builder.push(sym(s)); }

    inline void pop_reg(Reg r)                 { m_builder.pop(reg(r)); }

    size_t typeid_of(const DataType& type) noexcept {
        if (!type.root().is_object) {
            SimpleDataType stype = type.root().getsimpletype();
            switch (stype) {
            case SimpleDataType::_int:       return TYPEID_INT;
            case SimpleDataType::_void:      return TYPEID_VOID;
            case SimpleDataType::ptr:        return TYPEID_PTR;
            case SimpleDataType::any:        return TYPEID_ANY;
            case SimpleDataType::_char:      return TYPEID_CHAR;
            case SimpleDataType::_constexpr: return 0ULL;
            case SimpleDataType::proc_ptr:   return 0ULL;
            }
        } else {
            GString sname = type.root().getobjectname();
            std::optional<Struct> st = struct_lookup(sname);
            if (st.has_value())  return st.value().m_typeid;
            std::optional<Interface> st2 = inter_lookup(sname);
            if (st2.has_value()) return st2.value().m_typeid;
        }
        assert(false);
        return 0;
    }

    size_t sizeof_of(DataType& type) noexcept {
        if (type.root().is_object) {
            GString name = type.root().getobjectname();
            std::optional<Struct> st = struct_lookup(name);
            if (st.has_value()) {
                return st.value().fields.size() * 4ULL;
            }
            std::optional<Interface> inter = inter_lookup(name);
            if (inter.has_value()) {
                return 4ULL;
            }
            return 0ULL;
        } else {
            return 4ULL;
        }
    }

    struct Var {
        GString name{};
        size_t      stack_loc{};
        DataType    type;

        GString ref() {
            return "dword [ebp-" + GString(std::to_string(stack_loc)) + "]";
        }
        MemRef mem() const {
            return MemRef::baseDisp(Reg::EBP, -static_cast<int32_t>(stack_loc));
        }
    };

    struct GVar {
        GString name;
        DataType    type;
    };

    struct Procedure {
        GString name{};
        GVector<std::pair<GString, DataType>> params{};
        DataType rettype;
        size_t   stack_allign;
        GVector<ProcAttr> attrs;
        Token    def;
        bool     prototype;
        GVector<Procedure*> overrides;
        bool     override;
        std::optional<int> uniq_sign;
        GVector<GString>* templates;
        const NodeScope*  scope;
        const NodeStmtProc* from;
        GMap<GString, bool> instanceated;
        GString mbn;
        int overload_nth = 0;

        GString get_sign() {
            if (params.empty()) {
                if (!uniq_sign.has_value()) uniq_sign = rand() % 1000;
                return GString(std::to_string(uniq_sign.value()));
            }
            GString res;
            for (size_t i = 0; i < params.size(); ++i) {
                res += params[i].second.sign();
            }
            return res;
        }

        void gen_ret(Generator& gen, std::optional<GString> cnm) {
            size_t allign = gen.__compute_allign_ret();
            if (allign != 0) {
                gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(allign * 4));
            }
            GString lbl = "__";
            if (cnm.has_value()) {
                lbl += gen.mangle_ns_name(cnm.value()) + "@";
            } else {
                if (!gen.m_tsigns.empty() && !mbn.empty())
                    lbl += mbn + "@";
            }
            lbl += name;
            if (!gen.m_tsigns.empty()) lbl += gen.m_tsigns.back();
            if (override)               lbl += get_sign();
            lbl += "@ret";

            gen.m_builder.jmp(gen.label(lbl));
        }

        void call(Generator& gen, const size_t allign) {
            gen.m_builder.call(gen.sym(name));
            if (allign != 0ULL) {
                gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(static_cast<int32_t>(allign)));
            }
        }
    };

    struct String {
        GString value{};
        size_t      index{};
    };

    struct Struct {
        GString name;
        GMap<GString, Field> fields;
        std::optional<GString> __allocator;
        GVector<std::pair<GString, DataType>> __fields;
        size_t  m_typeid;
        bool    temp;
        GVector<GString> temps;
        Token   def;
        std::optional<DataType> parent_type;

        size_t size_of() const noexcept {
            return __fields.size();
        }

        bool has_allocator() const noexcept {
            return __allocator.has_value();
        }

        void alloc(Generator& gen) const noexcept {
            gen.push_imm(static_cast<int32_t>(size_of() * 4ULL));
            if (has_allocator()) gen.m_builder.call(gen.sym(__allocator.value()));
            else                 gen.m_builder.call(gen.sym("memalloc"));
            gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(4));
        }

        void dealloc(Generator& gen) {
            gen.m_builder.call(gen.sym("memfree"));
            gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(4));
        }

        void call_dtor(Generator& gen, const GString& offset, const Token& def) const {
            if (has_allocator())
                gen.GeneratorWarning(def, "deleting object with custom allocator function.");
            std::optional<Procedure> __dtor = gen.proc_lookup(__DTOR_PREFIX + name);
            if (!__dtor.has_value()) return;
            gen.m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("push " + offset)));
            __dtor.value().call(gen, 4ULL);
        }

        void call_dtor_s(Generator& gen, const Token& def) const {
            if (has_allocator())
                gen.GeneratorWarning(def, "deleting object with custom allocator function.");
            std::optional<Procedure> __dtor = gen.proc_lookup(__DTOR_PREFIX + name);
            if (!__dtor.has_value()) return;
            gen.m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("push dword [esp]")));
            __dtor.value().call(gen, 4ULL);
        }
    };

    struct InterfaceMethodInfo {
        GString name;
        GVector<std::pair<GString, DataType>> params;
        DataType rettype;
    };

    struct Interface {
        GString name;
        GMap<GString, InterfaceMethodInfo> methods;
        GVector<GString> method_order;
        size_t m_typeid;
        GVector<GString> temps;
    };

    struct Namespace {
        GMap<GString, Procedure> procs;
        GMap<GString, int> consts;
        GString name;
        Token       def;
    };

    class Executor {
    public:
        class ReturnException {
        public:
            ReturnException(const int _value) : value(_value) {}
            int get_value() { return value; }
        private:
            int value;
        };

        Executor() = delete;
        Executor(Generator& _gen, Token& _where) : gen(_gen) {
            where = _where;
        }

        int execute_expr(const NodeExpr* expr) {
            return gen.eval(expr, where);
        }

        void execute_stmt(const NodeStmt* stmt) {
            if (std::holds_alternative<NodeStmtReturn*>(stmt->var)) {
                auto ret = std::get<NodeStmtReturn*>(stmt->var);
                if (!ret->expr.has_value()) {
                    gen.GeneratorError(ret->def, "return without value in compile-time execution.");
                }
                throw ReturnException(execute_expr(ret->expr.value()));
            } else {
                gen.GeneratorError(where, "procedure is not constant-evaluatable.");
            }
        }

        void execute(Procedure& proc) {
            try {
                for (NodeStmt* stmt : proc.from->scope->stmts) {
                    execute_stmt(stmt);
                }
            } catch (...) {
                throw;
            }
        }

        std::optional<int> var_lookup(__str_ref name) noexcept {
            for (int i = static_cast<int>(m_vars.size()) - 1; i > -1; --i) {
                const auto& search = m_vars[i].find(name);
                if (search != m_vars[i].end()) {
                    return search->second;
                }
            }
            return std::nullopt;
        }
    private:
        GVector<GMap<GString, int>> m_vars;
        Token where;
        Generator& gen;
    };

    explicit Generator(NodeProg* prog, DiagnosticManager* dman, ArenaAllocator* arenall) {
        m_prog = prog;
        m_diag_man = dman;
        m_allocator = arenall;
    }

    Generator(const Generator& other) {
        m_typeid_table   = other.m_typeid_table;
        m_consts         = other.m_consts;
        m_typedefs       = other.m_typedefs;
        m_string_index   = other.m_string_index;
        m_prog           = other.m_prog;
        m_strings        = other.m_strings;
        m_procs          = other.m_procs;
        m_structs        = other.m_structs;
        m_global_vars    = other.m_global_vars;
        m_interfaces     = other.m_interfaces;
        m_used_labels    = other.m_used_labels;
        m_namespaces     = other.m_namespaces;
        m_cur_namespace  = NULL;
        m_allocator      = other.m_allocator;
        m_diag_man       = other.m_diag_man;
        m_structs_count  = other.m_structs_count;
        CTX_IOTA         = other.CTX_IOTA;
        m_label_count    = other.m_label_count;
        m_typeid_table_size = other.m_typeid_table_size;
    
        m_main_ir  = other.m_main_ir;
        m_templ_ir = other.m_templ_ir;
        m_builder  = IRBuilder(m_main_ir); 
        m_pending_templates = other.m_pending_templates;
    }

    std::optional<Var> var_lookup_cs(__str_ref name) noexcept {
        GMap<GString, Var>& vrs = last_scope();
        const auto& search = vrs.find(name);
        if (search != vrs.end()) return search->second;
        return std::nullopt;
    }

    std::optional<Var> var_lookup(__str_ref name) noexcept {
        for (int i = static_cast<int>(m_vars.size()) - 1; i > -1; --i) {
            const auto& search = m_vars[i].find(name);
            if (search != m_vars[i].end()) {
                return search->second;
            }
        }
        return std::nullopt;
    }

    std::optional<DataType> typedef_lookup(__str_ref name) noexcept {
        for (int i = static_cast<int>(m_typedefs.size()) - 1; i > -1; --i) {
            const auto& search = m_typedefs[i].find(name);
            if (search != m_typedefs[i].end()) {
                return search->second;
            }
        }
        return std::nullopt;
    }

    Var var_lookup_err(__str_ref name, const Token& def) noexcept {
        const std::optional<Var> v = var_lookup(name);
        if (!v.has_value()) GeneratorError(def, "unkown variable `" + name + "`");
        return v.value();
    }

    GMap<GString, Var>& last_scope() {
        return m_vars[m_vars.size() - 1ULL];
    }

    GMap<GString, Constant>& last_scope_cns() {
        return m_consts[m_consts.size() - 1ULL];
    }

    GMap<GString, DataType>& last_scope_tdef() {
        return m_typedefs[m_typedefs.size() - 1ULL];
    }

    std::optional<Constant> const_lookup(__str_ref name) noexcept {
        for (int i = static_cast<int>(m_consts.size()) - 1; i > -1; --i) {
            const auto& search = m_consts[i].find(name);
            if (search != m_consts[i].end()) {
                return search->second;
            }
        }
        return std::nullopt;
    }

    std::optional<GVar> gvar_lookup(__str_ref name) noexcept {
        const auto& search = m_global_vars.find(name);
        if (search != m_global_vars.end()) {
            return search->second;
        }
        return std::nullopt;
    }

    std::optional<Interface> inter_lookup(__str_ref name) {
        const auto& search = m_interfaces.find(name);
        if (search != m_interfaces.end()) {
            return search->second;
        }
        return std::nullopt;
    }

    std::optional<Procedure> proc_lookup(__str_ref name) noexcept {
        const auto& search = m_procs.find(name);
        if (search != m_procs.end()) {
            return search->second;
        }
        return std::nullopt;
    }

    std::optional<Struct> struct_lookup(__str_ref name) noexcept {
        const auto& search = m_structs.find(name);
        if (search != m_structs.end()) {
            return search->second;
        }
        return std::nullopt;
    }

    std::optional<Namespace*> namespace_lookup(__str_ref name) noexcept {
        const auto& search = m_namespaces.find(name);
        if (search != m_namespaces.end()) {
            return search->second;
        }
        return std::nullopt;
    }

    std::optional<Field> field_lookup(const Struct& st, __str_ref field) const noexcept {
        const auto& search = st.fields.find(field);
        if (search != st.fields.end()) {
            return search->second;
        }
        return std::nullopt;
    }

    std::optional<String> string_lookup(__str_ref svalue) noexcept {
        const auto& search = m_strings.find(svalue);
        if (search != m_strings.end()) {
            return search->second;
        }
        return std::nullopt;
    }

    void DiagnosticMessage(const Token& tok, __str_ref header, __str_ref msg, const int col_inc) {
        m_diag_man->DiagnosticMessage(tok, header, msg, col_inc);
    }

    void GeneratorError(const Token& tok, __str_ref msg) {
        m_diag_man->DiagnosticMessage(tok, "error", msg, 0);

        if (m_cur_proc.has_value()) {
            const Procedure& p = m_cur_proc.value();
            GString detail = "in function `" + p.name + "`";
            m_diag_man->DiagnosticMessage(p.def, "note", detail, 0, false);
        }

        exit(EXIT_FAILURE);
    }

    void GeneratorWarning(const Token& tok, __str_ref msg) {
        m_diag_man->DiagnosticMessage(tok, "warning", msg, 0);
    }

    GVector<DataType> get_template_args(const DataType& dt) {
        return dt.node->generics;
    }

    DataType canonical_type(const DataType& src) {
        if (!src.is_object()) return src;
        
        std::optional<Struct> st = struct_lookup(src.getobjectname());
        if (!st.has_value() || !st.value().temp) {
            DataType clean_copy = src;
            DataType res(src.root()); 
            return res;
        }
        return src;
    }

    bool is_base_of(const DataType& base, const DataType& derived) {
        if (!base.is_object() || !derived.is_object()) return false;
        
        if (base == derived) return true;
        
        std::optional<Struct> st_opt = struct_lookup(derived.getobjectname());
        if (!st_opt.has_value()) return false;
        
        if (!st_opt.value().parent_type.has_value()) return false;
        
        DataType parent = st_opt.value().parent_type.value();
        
        if (st_opt.value().temp) {
            GVector<DataType> targs = get_template_args(derived);
            GMap<GString, DataType> temps = compute_temps(st_opt.value().temps, targs);
            substitute_template_wct(parent, temps);
        }
        
        if (parent == base) return true;
        return is_base_of(base, parent);
    }

    bool same_param_types(const GVector<std::pair<GString, DataType>>& a,
                          const GVector<std::pair<GString, DataType>>& b) {
        if (a.size() != b.size()) return false;
        for (size_t i = 0; i < a.size(); ++i) {
            if (a[i].second != b[i].second) return false;
        }
        return true;
    }

    bool proc_same_signature(const NodeStmtProc* def, const Procedure& p) {
        if (p.rettype != def->rettype) return false;
        if (!same_param_types(p.params, def->params)) return false;
        bool t1 = (p.templates != NULL);
        bool t2 = (def->templates != NULL);
        if (t1 != t2) return false;
        if (t1 && t2) {
            if (*p.templates != *def->templates) return false;
        }
        return true;
    }

    NodeExpr* make_ident_expr(GString name) {
        auto* term = m_allocator->emplace<NodeTermIdent>();
        term->ident = {TokenType_t::ident, 0, 0, name, "", std::nullopt}; 
        auto* t = m_allocator->emplace<NodeTerm>(); t->var = term;
        auto* e = m_allocator->emplace<NodeExpr>(); e->var = t;
        return e;
    }

    NodeExpr* make_neq_expr(NodeExpr* lhs, NodeExpr* rhs, Token def) {
        auto* nq = m_allocator->emplace<NodeBinExprNotEq>();
        nq->lhs = lhs;
        nq->rhs = rhs;
        auto* be = m_allocator->emplace<NodeBinExpr>();
        be->def = def;
        be->var = nq;
        auto* e = m_allocator->emplace<NodeExpr>(); e->var = be;
        return e;
    }

    NodeExpr* make_int_lit(int val) {
        auto* term = m_allocator->emplace<NodeTermIntLit>();
        term->int_lit = {TokenType_t::int_lit, 0, 0, GString(std::to_string(val)), "", std::nullopt};
        auto* t = m_allocator->emplace<NodeTerm>(); t->var = term;
        auto* e = m_allocator->emplace<NodeExpr>(); e->var = t;
        return e;
    }

    NodeExpr* make_method_call(NodeExpr* obj, GString method, GVector<NodeExpr*> args, Token def) {
        auto* call = m_allocator->emplace<NodeTermCall>();
        call->def = def;
        call->name = method;
        if (!args.empty()) {
            auto* bargs = m_allocator->emplace<NodeBinExprArgs>();
            bargs->args = args;
            auto* be = m_allocator->emplace<NodeBinExpr>(); be->var = bargs;
            auto* ae = m_allocator->emplace<NodeExpr>(); ae->var = be;
            call->args = ae;
        } else {
            call->args = std::nullopt;
        }
        
        auto* t_call = m_allocator->emplace<NodeTerm>(); t_call->var = call;
        auto* e_rhs = m_allocator->emplace<NodeExpr>(); e_rhs->var = t_call;
        
        auto* dot = m_allocator->emplace<NodeBinExprDot>();
        dot->lhs = obj;
        dot->rhs = e_rhs;
        
        auto* b_dot = m_allocator->emplace<NodeBinExpr>();
        b_dot->def = def;
        b_dot->var = dot;
        
        auto* e = m_allocator->emplace<NodeExpr>(); e->var = b_dot;
        return e;
    }
    
    NodeExpr* make_deref_expr(NodeExpr* expr, Token def) {
        auto* unref = m_allocator->emplace<NodeTermUnref>();
        unref->def = def;
        unref->expr = expr;
        auto* t = m_allocator->emplace<NodeTerm>(); t->var = unref;
        auto* e = m_allocator->emplace<NodeExpr>(); e->var = t;
        return e;
    }
    
    NodeStmt* make_inc_stmt(NodeExpr* lvalue, NodeExpr* expr, Token def) {
        auto* inc = m_allocator->emplace<NodeStmtIncBy>();
        inc->def = def;
        inc->lvalue = lvalue;
        inc->expr = expr;
        auto* s = m_allocator->emplace<NodeStmt>(); s->var = inc;
        return s;
    }

    DataType type_of_dot(const NodeBinExprDot* dot, const Token& def) {
        DataType otype = type_of_expr(dot->lhs);

        if (!std::holds_alternative<NodeTerm*>(dot->rhs->var)) {
            return BaseDataTypeVoid;
        }

        NodeTerm* term = std::get<NodeTerm*>(dot->rhs->var);

        if (std::holds_alternative<NodeTermCall*>(term->var)) {
            NodeTermCall* call = std::get<NodeTermCall*>(term->var);
            GString mname = call->name;

            if (is_interface_type(otype)) {
                Interface iface = get_interface(otype, def);
                const auto it = iface.methods.find(mname);
                if (it == iface.methods.end()) {
                    GeneratorError(def,
                        "interface `" + iface.name + "` has no method `" + mname + "`");
                }
                const InterfaceMethodInfo& im = it->second;
                return im.rettype;
            }

            NodeTermMtCall mt;
            mt.def   = call->def;
            mt.mt    = dot->lhs;
            mt.name  = call->name;
            mt.targs = call->targs;

            GVector<NodeExpr*> allargs;
            allargs.push_back(dot->lhs);

            if (call->args.has_value()) {
                NodeExpr* aexpr = call->args.value();
                if (std::holds_alternative<NodeBinExpr*>(aexpr->var) &&
                    std::holds_alternative<NodeBinExprArgs*>(
                        std::get<NodeBinExpr*>(aexpr->var)->var))
                {
                    NodeBinExpr* be = std::get<NodeBinExpr*>(aexpr->var);
                    NodeBinExprArgs* barg = std::get<NodeBinExprArgs*>(be->var);
                    auto& raw = barg->args;
                    allargs.insert(allargs.end(), raw.begin(), raw.end());
                } else {
                    allargs.push_back(aexpr);
                }
            }

            NodeBinExprArgs bargs{ .args = allargs };
            NodeBinExpr     bexpr{ .def = call->def, .var = &bargs };
            NodeExpr        eargs{ .var = &bexpr };

            mt.args = &eargs;

            NodeTerm wrapTerm{ .var = &mt };
            NodeExpr wrapExpr{ .var = &wrapTerm };

            return type_of_expr(&wrapExpr);
        }

        if (!std::holds_alternative<NodeTermIdent*>(term->var)) {
            return BaseDataTypeVoid;
        }

        NodeTermIdent* tid = std::get<NodeTermIdent*>(term->var);
        Token ident = tid->ident;
        GString field_name = ident.value.value();

        if (!otype.root().is_object) {
            return BaseDataTypeVoid;
        }

        GString struct_name = otype.root().getobjectname();
        std::optional<Struct> st = struct_lookup(struct_name);
        if (st.has_value()) {
            std::optional<Field> field = field_lookup(st.value(), field_name);
            if (!field.has_value()) {
                GeneratorError(def,
                    "struct `" + struct_name +
                    "` don't have field `" + field_name + "`");
            }
            Struct stc = st.value();
            Field  fd  = field.value();
            if (stc.temp) {
                GVector<DataType> targs;
                targs = otype.node->generics;

                if (targs.size() != stc.temps.size()) {
                    GeneratorError(def, 
                        "Internal Compiler Error: template args count mismatch for struct `" 
                        + stc.name + "` (expected " + GString(std::to_string(stc.temps.size())) 
                        + ", got " + GString(std::to_string(targs.size())) + ")");
                }

                GMap<GString, DataType> temps = compute_temps(stc.temps, targs);
                substitute_template_wct(fd.type, temps);
            }
            return fd.type;
        }

        std::optional<Interface> inter = inter_lookup(struct_name);
        if (inter.has_value()) {
            GeneratorError(def,
                "interfaces have no fields; cannot access `" + field_name +
                "` on interface `" + struct_name + "`");
        }

        return BaseDataTypeVoid;
    }

    DataType type_of_expr(const NodeExpr* expr) {
        DataType res = __type_of_expr(expr);
        substitute_template(res);
        res = canonical_type(res);

        if (m_temps.empty()) {
            expr->cached_type = res;
        }
        return res;
    }

    DataType __type_of_expr(const NodeExpr* expr) {
        if (holds_alternative<NodeTerm*>(expr->var)) {
            NodeTerm* term = std::get<NodeTerm*>(expr->var);
            if (std::holds_alternative<NodeTermIntLit*>(term->var)) {
                return BaseDataTypeInt;
            }
            if (std::holds_alternative<NodeTermCtEval*>(term->var)) {
                return type_of_expr(std::get<NodeTermCtEval*>(term->var)->expr);
            }
            if (std::holds_alternative<NodeTermLine*>(term->var)) {
                return BaseDataTypeInt;
            }
            if (std::holds_alternative<NodeTermCol*>(term->var)) {
                return BaseDataTypeInt;
            }
            if (std::holds_alternative<NodeTermCtMdefined*>(term->var)) {
                return BaseDataTypeInt;
            }
            if (std::holds_alternative<NodeTermFile*>(term->var)) {
                return BaseDataTypePtr;
            }
            if (std::holds_alternative<NodeTermStrLit*>(term->var)) {
                BaseDataType tp = BaseDataTypeChar;
                tp.ptrlvl += 1;
                return tp;
            }
            if (std::holds_alternative<NodeTermCast*>(term->var)) {
                return std::get<NodeTermCast*>(term->var)->type;
            }
            if (std::holds_alternative<NodeTermMtCall*>(term->var)) {
                NodeTermMtCall* term_call = std::get<NodeTermMtCall*>(term->var);
                DataType tpof = type_of_expr(term_call->mt);

                if (is_interface_type(tpof)) {
                    Interface iface = get_interface(tpof, term_call->def);
                    const auto it = iface.methods.find(term_call->name);
                    if (it == iface.methods.end()) {
                        GeneratorError(term_call->def,
                            "interface `" + iface.name + "` has no method `" +
                            term_call->name + "`");
                    }
                    const InterfaceMethodInfo& im = it->second;
                    return im.rettype;
                }

                if (!tpof.root().is_object || tpof.root().link)
                    GeneratorError(term_call->def,
                        "can't call method from type " + tpof.to_string() + ".");

                NodeTermNmCall nmcall;
                nmcall.def  = term_call->def;
                nmcall.nm   = tpof.root().getobjectname();
                nmcall.name = term_call->name;
                nmcall.args = term_call->args;
                nmcall.targs = term_call->targs;

                NodeTerm aterm;
                aterm.var = &nmcall;
                NodeExpr aex;
                aex.var = &aterm;
                return type_of_expr(&aex);
            }
            if (std::holds_alternative<NodeTermRd*>(term->var)) {
                return BaseDataTypeInt;
            }
            if (std::holds_alternative<NodeTermParen*>(term->var)) {
                return type_of_expr(std::get<NodeTermParen*>(term->var)->expr);
            }
            if (std::holds_alternative<NodeTermUnref*>(term->var)) {
                NodeTermUnref* unref = std::get<NodeTermUnref*>(term->var);
                DataType tp = type_of_expr(unref->expr);
                
                if (!tp.root().link && tp.root().ptrlvl == 0 && !tp.is_object() && tp.root().getsimpletype() != SimpleDataType::ptr) {
                    GeneratorError(unref->def, "can't dereference not-reference or pointer type.");
                }
                
                if (tp.root().link) {
                    DataType res(tp.root());
                    res.node->generics = tp.node->generics;
                    res.root().link = false; 
                    return res;
                }
                if (tp.root().ptrlvl != 0ULL) {
                    DataType res(tp.root());
                    res.node->generics = tp.node->generics;
                    res.root().ptrlvl--;
                    return res;
                }

                if (tp.is_object()) {
                    NodeTermMtCall deref;
                    deref.def = unref->def;
                    deref.mt = unref->expr;
                    deref.name = "m_deref";
                    GVector<NodeExpr*> args;
                    args.push_back(unref->expr);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = unref->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    deref.args = &eargs;
                    NodeTerm asterm{ .var = &deref };
                    NodeExpr asexpr{ .var = &asterm };
                    return type_of_expr(&asexpr);
                }
                return tp;
            }
            if (std::holds_alternative<NodeTermCastTo*>(term->var)) {
                return type_of_expr(std::get<NodeTermCastTo*>(term->var)->to);
            }
            if (std::holds_alternative<NodeTermSizeof*>(term->var)) {
                return BaseDataTypeInt;
            }
            if (std::holds_alternative<NodeTermExprStmt*>(term->var)) {
                return type_of_expr(std::get<NodeTermExprStmt*>(term->var)->expr);
            }
            if (std::holds_alternative<NodeTermTypeid*>(term->var)) {
                return BaseDataTypeInt;
            }
            if (std::holds_alternative<NodeTermPop*>(term->var)) {
                return BaseDataTypeAny;
            }
            if (std::holds_alternative<NodeTermAmpersand*>(term->var)) {
                DataType tp = type_of_expr(std::get<NodeTermAmpersand*>(term->var)->expr);
                
                DataType res(tp.root());
                res.node->generics = tp.node->generics;
                res.root().ptrlvl += 1;
                
                return res;
            }
            if (std::holds_alternative<NodeTermDrvalue*>(term->var)) {
                DataType tp = type_of_expr(std::get<NodeTermDrvalue*>(term->var)->expr);
                if (!tp.root().link) {
                    GeneratorError(std::get<NodeTermDrvalue*>(term->var)->def, "__disable_rvalue__ on a non-rvalue expression");
                }
                
                DataType res(tp.root());
                res.node->generics = tp.node->generics;
                res.root().link = false;
                
                return res;
            }
            if (std::holds_alternative<NodeTermCall*>(term->var)) {
                NodeTermCall* call = std::get<NodeTermCall*>(term->var);
                GString name = call->name;
                std::optional<Var> var = var_lookup(name);
                if (var.has_value() && 
                    var.value().type.root().is_simple() &&
                    var.value().type.root().getsimpletype() == SimpleDataType::proc_ptr) {
                    if (var.value().type.node->generics.empty()) return BaseDataTypeVoid;
                    return var.value().type.node->generics[0];
                }
                std::optional<Procedure> _proc = proc_lookup(name);
                if (_proc.has_value()) {
                    Procedure proc = _proc.value();
                    if (proc.rettype == BaseDataTypeConst) return BaseDataTypeInt;
                    if (!proc.overrides.empty()) {
                        resolve_overrides_tp(&proc, call->args, call->def, call->targs);
                    }
                    if (proc.templates == NULL || !_proc.value().rettype.root().is_object) {
                        return proc.rettype;
                    }
                    GMap<GString, DataType> temps;
                    bool substituted = false;
                    if (call->targs.empty() && proc.templates == NULL) return proc.rettype;
                    else if (call->targs.empty() && call->args.has_value() && proc.templates != NULL) {
                        temps = try_derive_templates(call->targs, proc.params, call->def, proc.templates, __getargs(call->args.value()), proc);
                        substituted = true;
                    }
                    size_t counter{ 0 };
                    if (!substituted) {
                        if (proc.templates != NULL && call->targs.size() != proc.templates->size()) {
                            GeneratorError(call->def, "procedure `" + call->name + "` expects " + GString(std::to_string(proc.templates->size())) + " template arguments, but got " + GString(std::to_string(call->targs.size())));
                        }
                        for (auto&& el : *proc.templates) {
                            temps[el] = call->targs[counter++];
                        }
                    }
                    const auto& search = temps.find(proc.rettype.root().to_string_d());
                    if (search != temps.end()) proc.rettype = search->second;
                    return proc.rettype;
                }
                std::optional<Struct> _st = struct_lookup(call->name);
                if (_st.has_value()) {
                    Struct st = _st.value();
                    BaseDataType bs = st.name;
                    DataType dt = bs;
                    if (st.temp && call->targs.size() != st.temps.size())
                        GeneratorError(call->def, "struct `" + st.name + "` except " + GString(std::to_string(st.temps.size())) + " template arguments in <...>, bug got " + GString(std::to_string(call->targs.size())) + ".");
                    for (int i = 0; i < static_cast<int>(call->targs.size()); ++i) {
                        DataType arg_tp = call->targs[i];
                        substitute_template(arg_tp);
                        
                        dt.node->generics.push_back(arg_tp);
                    }
                    return dt;
                }
                GeneratorError(call->def, "unkown procedure `" + name + "`");
                return BaseDataTypeVoid;
            }
            if (std::holds_alternative<NodeTermNmCall*>(term->var)) {
                NodeTermNmCall* call = std::get<NodeTermNmCall*>(term->var);
                __str_ref name = call->name;
                __str_ref nmsp = call->nm;
                std::optional<Namespace*> nms = namespace_lookup(nmsp);
                if (!nms.has_value()) GeneratorError(call->def, "unkown namespace `" + nmsp + "`");
                const auto& search = nms.value()->procs.find(name);
                if (search == nms.value()->procs.end()) GeneratorError(call->def, "namespace `" + nmsp + "` doesn't have procedure `" + name + "`");
                Procedure proc = search->second;
                if (!proc.overrides.empty()) {
                    resolve_overrides_tp(&proc, call->args, call->def, call->targs);
                }
                if (proc.templates == NULL || !proc.rettype.root().is_object) {
                    return proc.rettype;
                }
                GMap<GString, DataType> temps;
                bool substituted = false;
                if (call->targs.empty() && proc.templates == NULL) return proc.rettype;
                else if (call->targs.empty() && call->args.has_value() && proc.templates != NULL) {
                    temps = try_derive_templates(call->targs, proc.params, call->def, proc.templates, __getargs(call->args.value()), proc);
                    substituted = true;
                }
                size_t counter{ 0 };
                if (!substituted) {
                    if (proc.templates != NULL && call->targs.size() != proc.templates->size()) {
                        GeneratorError(call->def, "procedure `" + call->name + "` expects " + GString(std::to_string(proc.templates->size())) + " template arguments, but got " + GString(std::to_string(call->targs.size())));
                    }
                    for (auto&& el : *proc.templates) {
                        temps[el] = call->targs[counter++];
                    }
                }
                substitute_template_wct(proc.rettype, temps);
                return proc.rettype;
            }
            if (std::holds_alternative<NodeTermIdent*>(term->var)) {
                std::optional<Var> svar = var_lookup(std::get<NodeTermIdent*>(term->var)->ident.value.value());
                if (svar.has_value()) {
                    return svar.value().type;
                }
                std::optional<GVar> glvar = gvar_lookup(std::get<NodeTermIdent*>(term->var)->ident.value.value());
                if (glvar.has_value()) {
                    return glvar.value().type;
                }
                std::optional<Constant> scns = const_lookup(std::get<NodeTermIdent*>(term->var)->ident.value.value());
                if (scns.has_value()) {
                    return BaseDataTypeInt;
                }
                std::optional<Procedure> prc = proc_lookup(std::get<NodeTermIdent*>(term->var)->ident.value.value());
                if (prc.has_value()) {
                    DataType tp(BaseDataTypeProcPtr);
                    
                    tp.node->generics.push_back(prc.value().rettype);
                    
                    for(const auto& param : prc.value().params) {
                        tp.node->generics.push_back(param.second);
                    }
                    return tp;
                }
                GeneratorError(std::get<NodeTermIdent*>(term->var)->ident, "unkown word `" + std::get<NodeTermIdent*>(term->var)->ident.value.value() + "`");
            }
            if (std::holds_alternative<NodeTermNmIdent*>(term->var)) {
                return BaseDataTypeInt; // TODO: introduce enum type
            }
        }
        if (holds_alternative<NodeBinExpr*>(expr->var)) {
            NodeBinExpr* binex = std::get<NodeBinExpr*>(expr->var);
            if (std::holds_alternative<NodeBinExprAdd*>(binex->var)) {
                auto add = std::get<NodeBinExprAdd*>(binex->var);
                DataType tp = type_of_expr(add->lhs);
                if (tp.is_object()) {
                    NodeTermMtCall mtadd;
                    mtadd.def = binex->def;
                    mtadd.mt = add->lhs;
                    mtadd.name = "m_add";
                    GVector<NodeExpr*> args;
                    args.push_back(add->lhs);
                    args.push_back(add->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = binex->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    mtadd.args = &eargs;
                    NodeTerm asterm{ .var = &mtadd };
                    NodeExpr asexpr{ .var = &asterm };
                    return type_of_expr(&asexpr);
                }
                return type_of_expr(add->lhs);
            }
            if (std::holds_alternative<NodeBinExprMulti*>(binex->var)) {
                auto mul = std::get<NodeBinExprMulti*>(binex->var);
                DataType tp = type_of_expr(mul->lhs);
                if (tp.is_object()) {
                    NodeTermMtCall mtmul;
                    mtmul.def = binex->def;
                    mtmul.mt = mul->lhs;
                    mtmul.name = "m_mul";
                    GVector<NodeExpr*> args;
                    args.push_back(mul->lhs);
                    args.push_back(mul->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = binex->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    mtmul.args = &eargs;
                    NodeTerm asterm{ .var = &mtmul };
                    NodeExpr asexpr{ .var = &asterm };
                    return type_of_expr(&asexpr);
                }
                return type_of_expr(mul->lhs);
            }
            if (std::holds_alternative<NodeBinExprSub*>(binex->var)) {
                auto sub = std::get<NodeBinExprSub*>(binex->var);
                DataType tp = type_of_expr(sub->lhs);
                if (tp.is_object()) {
                    NodeTermMtCall mtsub;
                    mtsub.def = binex->def;
                    mtsub.mt = sub->lhs;
                    mtsub.name = "m_sub";
                    GVector<NodeExpr*> args;
                    args.push_back(sub->lhs);
                    args.push_back(sub->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = binex->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    mtsub.args = &eargs;
                    NodeTerm asterm{ .var = &mtsub };
                    NodeExpr asexpr{ .var = &asterm };
                    return type_of_expr(&asexpr);
                }
                return type_of_expr(sub->lhs);
            }
            if (std::holds_alternative<NodeBinExprDiv*>(binex->var)) {
                auto div = std::get<NodeBinExprDiv*>(binex->var);
                DataType tp = type_of_expr(div->lhs);
                if (tp.is_object()) {
                    NodeTermMtCall mtdiv;
                    mtdiv.def = binex->def;
                    mtdiv.mt = div->lhs;
                    mtdiv.name = "m_div";
                    GVector<NodeExpr*> args;
                    args.push_back(div->lhs);
                    args.push_back(div->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = binex->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    mtdiv.args = &eargs;
                    NodeTerm asterm{ .var = &mtdiv };
                    NodeExpr asexpr{ .var = &asterm };
                    return type_of_expr(&asexpr);
                }
                return type_of_expr(div->lhs);
            }
            if (std::holds_alternative<NodeBinExprMod*>(binex->var)) {
                return type_of_expr(std::get<NodeBinExprMod*>(binex->var)->lhs);
            }
            if (std::holds_alternative<NodeBinExprEqEq*>(binex->var)) {
                auto eqeq = std::get<NodeBinExprEqEq*>(binex->var);
                DataType tp = type_of_expr(eqeq->lhs);
                if (tp.is_object()) {
                    NodeTermMtCall mteqeq;
                    mteqeq.def = binex->def;
                    mteqeq.mt = eqeq->lhs;
                    mteqeq.name = "m_equal";
                    GVector<NodeExpr*> args;
                    args.push_back(eqeq->lhs);
                    args.push_back(eqeq->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = binex->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    mteqeq.args = &eargs;
                    NodeTerm asterm{ .var = &mteqeq };
                    NodeExpr asexpr{ .var = &asterm };
                    return type_of_expr(&asexpr);
                }
                return BaseDataTypeInt;
            }
            if (std::holds_alternative<NodeBinExprNotEq*>(binex->var)) {
                auto nq = std::get<NodeBinExprNotEq*>(binex->var);
                DataType tp = type_of_expr(nq->lhs);
                if (tp.is_object()) {
                    NodeTermMtCall mnq;
                    mnq.def = binex->def;
                    mnq.mt = nq->lhs;
                    mnq.name = "m_not_equal";
                    GVector<NodeExpr*> args;
                    args.push_back(nq->lhs);
                    args.push_back(nq->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = binex->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    mnq.args = &eargs;
                    NodeTerm asterm{ .var = &mnq };
                    NodeExpr asexpr{ .var = &asterm };
                    return type_of_expr(&asexpr);
                }
                return BaseDataTypeInt;
            }
            if (std::holds_alternative<NodeBinExprLess*>(binex->var)) {
                auto less = std::get<NodeBinExprLess*>(binex->var);
                DataType tp = type_of_expr(less->lhs);
                if (tp.is_object()) {
                    NodeTermMtCall mtless;
                    mtless.def = binex->def;
                    mtless.mt = less->lhs;
                    mtless.name = "m_less";
                    GVector<NodeExpr*> args;
                    args.push_back(less->lhs);
                    args.push_back(less->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = binex->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    mtless.args = &eargs;
                    NodeTerm asterm{ .var = &mtless };
                    NodeExpr asexpr{ .var = &asterm };
                    return type_of_expr(&asexpr);
                }
                return type_of_expr(less->lhs);
            }
            if (std::holds_alternative<NodeBinExprAbove*>(binex->var)) {
                auto above = std::get<NodeBinExprAbove*>(binex->var);
                DataType tp = type_of_expr(above->lhs);
                if (tp.is_object()) {
                    NodeTermMtCall mtabove;
                    mtabove.def = binex->def;
                    mtabove.mt = above->lhs;
                    mtabove.name = "m_above";
                    GVector<NodeExpr*> args;
                    args.push_back(above->lhs);
                    args.push_back(above->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = binex->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    mtabove.args = &eargs;
                    NodeTerm asterm{ .var = &mtabove };
                    NodeExpr asexpr{ .var = &asterm };
                    return type_of_expr(&asexpr);
                }
                return type_of_expr(above->lhs);
            }
            if (std::holds_alternative<NodeBinExprAnd*>(binex->var)) {
                return type_of_expr(std::get<NodeBinExprAnd*>(binex->var)->lhs);
            }
            if (std::holds_alternative<NodeBinExprOr*>(binex->var)) {
                return type_of_expr(std::get<NodeBinExprOr*>(binex->var)->lhs);
            }
            if (std::holds_alternative<NodeBinExprShr*>(binex->var)) {
                auto shr = std::get<NodeBinExprShr*>(binex->var);
                DataType tp = type_of_expr(shr->lhs);
                if (tp.is_object()) {
                    NodeTermMtCall mtshr;
                    mtshr.def = binex->def;
                    mtshr.mt = shr->lhs;
                    mtshr.name = "m_shr";
                    GVector<NodeExpr*> args;
                    args.push_back(shr->lhs);
                    args.push_back(shr->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = binex->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    mtshr.args = &eargs;
                    NodeTerm asterm{ .var = &mtshr };
                    NodeExpr asexpr{ .var = &asterm };
                    return type_of_expr(&asexpr);
                }
                return type_of_expr(shr->lhs);
            }
            if (std::holds_alternative<NodeBinExprShl*>(binex->var)) {
                auto shl = std::get<NodeBinExprShl*>(binex->var);
                DataType tp = type_of_expr(shl->lhs);
                if (tp.is_object()) {
                    NodeTermMtCall mtshl;
                    mtshl.def = binex->def;
                    mtshl.mt = shl->lhs;
                    mtshl.name = "m_shl";
                    GVector<NodeExpr*> args;
                    args.push_back(shl->lhs);
                    args.push_back(shl->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = binex->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    mtshl.args = &eargs;
                    NodeTerm asterm{ .var = &mtshl };
                    NodeExpr asexpr{ .var = &asterm };
                    return type_of_expr(&asexpr);
                }
                return type_of_expr(shl->lhs);
            }
            if (std::holds_alternative<NodeBinExprDot*>(binex->var)) {
                NodeBinExprDot* dot = std::get<NodeBinExprDot*>(binex->var);
                return type_of_dot(dot, binex->def);
            }
            if (std::holds_alternative<NodeBinExprIndex*>(binex->var)) {
                NodeBinExprIndex* idx = std::get<NodeBinExprIndex*>(binex->var);
                NodeTermMtCall call;
                call.def = binex->def;
                call.mt = idx->lhs;
                call.name = "m_index";
                GVector<NodeExpr*> args_list;
                args_list.push_back(idx->lhs);
                args_list.push_back(idx->rhs);
                NodeBinExprArgs bargs;
                bargs.args = args_list;
                NodeBinExpr be; be.var = &bargs;
                NodeExpr ae; ae.var = &be;
                call.args = &ae;
                NodeTerm t; t.var = &call;
                NodeExpr e; e.var = &t;
                return type_of_expr(&e);
            }
        }
        assert(false);
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

    void gen_push_str(__str_ref value) {
        std::optional<String> str = string_lookup(value);
        size_t idx = 0;
        if (!str.has_value()) {
            idx = (*m_string_index)++;
            m_strings[value] = { value, idx };
        } else {
            idx = str->index;
        }
        push_sym("s_" + GString(std::to_string(idx)));
    }

    void gen_traceback_push(Procedure& proc) {
        gen_push_str(proc.name);
        m_builder.call(sym("traceback_push"));
        m_builder.add(reg(Reg::ESP), imm(4));
    }

    void gen_traceback_push_nm(Procedure& proc, __str_ref nm) {
        gen_push_str(nm + "::" + proc.name);
        m_builder.call(sym("traceback_push"));
        m_builder.add(reg(Reg::ESP), imm(4));
    }

    void gen_term(const NodeTerm* term, bool lvalue = false)
    {
        struct TermVisitor {
            Generator& gen;
            bool       lvalue;

            void operator()(const NodeTermIntLit* term_int_lit) const {
                if (lvalue)
                    gen.GeneratorError(term_int_lit->int_lit, "can't use integer constant as lvalue expression (any operations that taking addres).");
                int32_t v = std::stol(term_int_lit->int_lit.value.value().c_str());
                gen.push_imm(v);
            }

            void operator()(const NodeTermType* tp) {
                gen.GeneratorError(tp->def, "`type` only can be used in context of compile-time expressions");
            }

            void operator()(const NodeTermCol* term_col) const {
                gen.push_imm(term_col->def.col);
            }

            void operator()(const NodeTermLine* term_line) const {
                gen.push_imm(term_line->def.line);
            }

            void operator()(const NodeTermPop* term_pop) const {
                consume_un(term_pop);
            }

            void operator()(const NodeTermExprStmt* term_stmt) const {
                gen.gen_scope(term_stmt->scope);
                gen.gen_expr(term_stmt->expr);
            }

            void operator()(const NodeTermFile* term_file) const {
                gen.gen_push_str(term_file->def.file);
            }

            void operator()(const NodeTermCtEval* term_eval) const {
                gen.GeneratorError(term_eval->def, "ct_eval is deprecated");
            }

            void operator()(const NodeTermCtMdefined* term_mdef) const {
                gen.push_imm(static_cast<int32_t>(term_mdef->value));
            }

            void operator()(NodeTermSizeof* term_sizeof) const {
                if (term_sizeof->expr.has_value()) {
                    DataType tp = gen.type_of_expr(term_sizeof->expr.value());
                    size_t   sz = gen.sizeof_of(tp);
                    if (!sz) {
                        gen.GeneratorError(term_sizeof->def, "please provide a type to sizeof, or close expression to parens.");
                    }
                    gen.push_imm(static_cast<int32_t>(sz));
                } else {
                    DataType dt = term_sizeof->type;
                    gen.substitute_template(dt);
                    size_t sz = gen.sizeof_of(dt);
                    if (!sz) {
                        gen.GeneratorError(term_sizeof->def, "please provide a type to sizeof, or close expression to parens.");
                    }
                    gen.push_imm(static_cast<int32_t>(sz));
                }
            }

            void operator()(const NodeTermRd* term_rd) const {
                gen.gen_expr(term_rd->expr);
                switch (term_rd->size) {
                case 8: {
                    gen.pop_reg(Reg::EDX);
                    Operand dst = gen.reg(Reg::ECX);
                    Operand m   = gen.mem(MemRef::baseDisp(Reg::EDX, 0));
                    gen.m_builder.emit(IRInstr(IROp::Load8, dst, m));
                    gen.push_reg(Reg::ECX);
                    break;
                }
                case 16: {
                    gen.pop_reg(Reg::EDX);
                    Operand dst = gen.reg(Reg::ECX);
                    Operand m   = gen.mem(MemRef::baseDisp(Reg::EDX, 0));
                    gen.m_builder.emit(IRInstr(IROp::Load16, dst, m));
                    gen.push_reg(Reg::ECX);
                    break;
                }
                case 32: {
                    gen.pop_reg(Reg::EDX);
                    Operand m = gen.mem(MemRef::baseDisp(Reg::EDX, 0));
                    gen.m_builder.mov(gen.reg(Reg::ECX), m);
                    gen.push_reg(Reg::ECX);
                    break;
                }
                default:
                    assert(false);
                }
            }

            void operator()(const NodeTermCast* term_cast) const {
                gen.gen_expr(term_cast->expr, lvalue);
            }

            void operator()(const NodeTermUnref* term_unref) const {
                DataType tp = gen.type_of_expr(term_unref->expr);
                if (!tp.root().link && tp.root().ptrlvl == 0 && !tp.is_object() && tp.root().getsimpletype() != SimpleDataType::ptr) {
                    gen.GeneratorError(term_unref->def, "can't dereference not-reference or pointer type `" + tp.to_string() + "`");
                }
                if (tp.is_object() && !tp.root().link && tp.root().ptrlvl == 0) {
                    if (lvalue) {
                        NodeTermMtCall deref;
                        deref.def = term_unref->def;
                        deref.mt = term_unref->expr;
                        deref.name = "m_assign_deref";
                        GVector<NodeExpr*> args;
                        args.push_back(term_unref->expr);
                        NodeBinExprArgs bargs{ .args = args };
                        NodeBinExpr bexpr{ .def = term_unref->def, .var = &bargs };
                        NodeExpr eargs{ .var = &bexpr };
                        deref.args = &eargs;
                        NodeTerm asterm{ .var = &deref };
                        NodeExpr asexpr{ .var = &asterm };
                        gen.gen_expr(&asexpr);
                    } else {
                        NodeTermMtCall deref;
                        deref.def = term_unref->def;
                        deref.mt = term_unref->expr;
                        deref.name = "m_deref";
                        GVector<NodeExpr*> args;
                        args.push_back(term_unref->expr);
                        NodeBinExprArgs bargs{ .args = args };
                        NodeBinExpr bexpr{ .def = term_unref->def, .var = &bargs };
                        NodeExpr eargs{ .var = &bexpr };
                        deref.args = &eargs;
                        NodeTerm asterm{ .var = &deref };
                        NodeExpr asexpr{ .var = &asterm };
                        gen.gen_expr(&asexpr);
                    }
                    return;
                }
                gen.gen_expr(term_unref->expr, lvalue);
                gen.pop_reg(Reg::EDX);
                Operand m = gen.mem(MemRef::baseDisp(Reg::EDX, 0));
                gen.m_builder.mov(gen.reg(Reg::ECX), m);
                gen.push_reg(Reg::ECX);
            }

            void operator()(const NodeTermCastTo* term_cast_to) const {
                gen.gen_expr(term_cast_to->expr, lvalue);
            }

            void operator()(NodeTermTypeid* term_typeid) const {
                if (term_typeid->ptype.has_value()) {
                    gen.push_imm(static_cast<int32_t>(gen.typeid_of(term_typeid->ptype.value())));
                } else {
                    gen.push_imm(static_cast<int32_t>(gen.typeid_of(gen.type_of_expr(term_typeid->expr))));
                }
            }

            void operator()(const NodeTermStrLit* term_str_lit) const {
                gen.gen_push_str(term_str_lit->str_lit);
            }

            void operator()(const NodeTermAmpersand* term_amp) const {
                gen.gen_expr(term_amp->expr, true);
            }

            void operator()(const NodeTermDrvalue* term_drval) const {
                if (lvalue)
                    gen.GeneratorError(term_drval->def, "using __disable_rvalue__ as rvalue");
                gen.gen_expr(term_drval->expr, false);
            }

            void operator()(const NodeTermIdent* term_ident) const {
                GString name = term_ident->ident.value.value();
                if (auto it = gen.var_lookup(name)) {
                    const Var& v = it.value();
                    if (lvalue) {
                        gen.m_builder.mov(gen.reg(Reg::EDX), gen.reg(Reg::EBP));
                        gen.m_builder.sub(gen.reg(Reg::EDX), gen.imm(static_cast<int32_t>(v.stack_loc)));
                        gen.push_reg(Reg::EDX);
                    } else {
                        gen.push_mem(v.mem());
                    }
                    return;
                }
                if (auto cns = gen.const_lookup(name)) {
                    if (lvalue) {
                        gen.GeneratorError(term_ident->ident, "can't get addres or reference of compile-time constant.");
                    }
                    gen.push_imm(cns->value);
                    return;
                }
                if (auto prc = gen.proc_lookup(name)) {
                    gen.push_sym(prc->name);
                    return;
                }
                if (auto ivar = gen.gvar_lookup(name)) {
                    if (lvalue) {
                        gen.push_sym("v_" + ivar->name);
                    } else {
                        gen.push_mem(gen.global_mem(ivar->name));
                    }
                    return;
                }
                gen.GeneratorError(term_ident->ident, "unkown word `" + name + "`");
            }

            void operator()(const NodeTermParen* term_paren) const {
                gen.gen_expr(term_paren->expr, lvalue);
            }

            void operator()(NodeTermNmCall* term_call) const {
                __str_ref pname = term_call->name;
                __str_ref nname = term_call->nm;
                std::optional<Namespace*> nms = gen.namespace_lookup(nname);
                if (!nms.has_value())
                    gen.GeneratorError(term_call->def, "unkown namespace `" + nname + "`");
                const auto& search = nms.value()->procs.find(pname);
                if (search == nms.value()->procs.end())
                    gen.GeneratorError(term_call->def, "namespace `" + nname + "` doesn't have procedure `" + pname + "`");
                Procedure proc = search->second;
                if (!proc.overrides.empty()) {
                    gen.resolve_overrides_tp(&proc, term_call->args, term_call->def, term_call->targs);
                }
                GVector<NodeExpr*> raw_args;
                if (term_call->args.has_value()) raw_args = gen.__getargs(term_call->args.value());
                
                auto inst_res = gen.instantiate_if_needed(proc, term_call->targs, raw_args, term_call->def, pname, nname);
                GString tsign = inst_res.first;
                auto temps = inst_res.second;

                if (proc.rettype.root() == BaseDataTypeVoid)
                    gen.GeneratorError(term_call->def, "can't use void " + nname + "::" + pname + "(...) as value");
                
                size_t stack_allign = 0;
                Procedure proc_check = proc;
                gen.substitute_template_params(temps, proc_check.params);

                if (!raw_args.empty() || proc.params.empty()) {
                    if (!raw_args.empty()) gen.__typecheck_call(raw_args, proc_check.params, term_call->def, proc_check, &stack_allign);
                } else {
                    gen.GeneratorError(term_call->def, "procedure `" + proc.name + "` expects " + GString(std::to_string(proc.params.size())) + " args, but got 0");
                }
                
                gen.gen_args(raw_args, proc_check.params, term_call->def);
                
                GString label = gen.mangle_ns_name(nname) + "@" + pname + tsign;
                if (proc.override) label += proc.get_sign();
                gen.m_builder.call(gen.sym(label));
                if (stack_allign != 0) {
                    gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(static_cast<int32_t>(stack_allign * 4)));
                }
                gen.push_reg(Reg::EAX);
            }

            void operator()(NodeTermCall* term_call) const {
                const GString name = term_call->name;
                std::optional<Var> var = gen.var_lookup(name);
                if (var.has_value() && 
                    var.value().type.root().is_simple() &&
                    var.value().type.root().getsimpletype() == SimpleDataType::proc_ptr) {
                    
                    DataType funcType = var.value().type;
                    const auto& gens = funcType.node->generics;
                    
                    if (gens.empty()) {
                        gen.GeneratorError(term_call->def, "ProcPtr without return type signature");
                    }
                    
                    DataType retType = gens[0];
                    GVector<DataType> paramTypes;
                    for(size_t i = 1; i < gens.size(); ++i) paramTypes.push_back(gens[i]);
                    
                    GVector<NodeExpr*> callArgs;
                    if (term_call->args.has_value()) callArgs = gen.__getargs(term_call->args.value());
                    
                    if (callArgs.size() != paramTypes.size()) {
                         gen.GeneratorError(term_call->def, "indirect call expects " + GString(std::to_string(paramTypes.size())) + " args, got " + GString(std::to_string(callArgs.size())));
                    }
                    
                    for (int i = static_cast<int>(callArgs.size()) - 1; i >= 0; --i) {
                        gen.gen_expr(callArgs[i]); 
                    }
                    
                    gen.m_builder.mov(gen.reg(Reg::EAX), gen.mem(var.value().mem()));
                    
                    gen.m_builder.call(gen.reg(Reg::EAX));
                    
                    if (!callArgs.empty()) {
                        gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(callArgs.size() * 4));
                    }
                    
                    gen.push_reg(Reg::EAX);
                    return;
                }
                
                std::optional<Procedure> _proc = gen.proc_lookup(name);
                if (_proc.has_value()) {
                    Procedure proc = _proc.value();
                    if (proc.rettype == BaseDataTypeConst) {
                        int res{ 0 };
                        Executor executor(gen, term_call->def);
                        try { executor.execute(proc); }
                        catch (Executor::ReturnException& ret) { res = ret.get_value(); }
                        gen.push_imm(res);
                        return;
                    }
                    if (!proc.overrides.empty()) {
                        gen.resolve_overrides_tp(&proc, term_call->args, term_call->def, term_call->targs);
                    }
                    if (proc.rettype.root() == BaseDataTypeVoid)
                        gen.GeneratorError(term_call->def, "can't use void " + term_call->name + "(...) as value");
                    GVector<NodeExpr*> raw_args;
                    if (term_call->args.has_value()) raw_args = gen.__getargs(term_call->args.value());
                    
                    auto inst_res = gen.instantiate_if_needed(proc, term_call->targs, raw_args, term_call->def, name, "");
                    GString tsign = inst_res.first;
                    auto temps = inst_res.second;
                    
                    size_t stack_allign = 0;
                    Procedure proc_check = proc;
                    gen.substitute_template_params(temps, proc_check.params);

                    if (!raw_args.empty() || proc.params.empty()) {
                        if (!raw_args.empty()) gen.__typecheck_call(raw_args, proc_check.params, term_call->def, proc_check, &stack_allign);
                    } else {
                        gen.GeneratorError(term_call->def, "procedure `" + proc.name + "` expects " + GString(std::to_string(proc.params.size())) + " args, but got 0");
                    }
                    
                    gen.gen_args(raw_args, proc_check.params, term_call->def);
                    
                    GString label = name + tsign;
                    if (proc.override) label += proc.get_sign();
                    gen.m_builder.call(gen.sym(label));
                    if (stack_allign != 0) {
                        gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(static_cast<int32_t>(stack_allign * 4)));
                    }
                    gen.push_reg(Reg::EAX);
                    return;
                }
                std::optional<Struct> st = gen.struct_lookup(term_call->name);
                if (st.has_value()) {
                    if (st.value().temp && term_call->targs.size() != st.value().temps.size())
                        gen.GeneratorError(term_call->def, "struct `" + st.value().name + "` except " + GString(std::to_string(st.value().temps.size()).c_str()) + " template arguments in <...>, bug got " + GString(std::to_string(term_call->targs.size()).c_str()) + ".");
                    size_t objectSize = st.value().fields.size();
                    if (objectSize == 0U) {
                        gen.GeneratorError(term_call->def, "try to allocate zero-sized type.");
                    }
                    gen.push_imm(static_cast<int32_t>(objectSize * 4U));
                    if (st.value().has_allocator()) {
                        gen.m_builder.call(gen.sym(st.value().__allocator.value()));
                    } else {
                        gen.m_builder.call(gen.sym("memalloc"));
                    }
                    gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(4));
                    bool eax_break = false;
                    GVector<NodeExpr*> iargs;
                    if (term_call->args.has_value()) {
                        if (std::holds_alternative<NodeBinExpr*>(term_call->args.value()->var)) {
                            NodeBinExpr* binargs = std::get<NodeBinExpr*>(term_call->args.value()->var);
                            if (std::holds_alternative<NodeBinExprArgs*>(binargs->var)) {
                                NodeBinExprArgs* args = std::get<NodeBinExprArgs*>(binargs->var);
                                iargs = args->args;
                            } else {
                                iargs.push_back(term_call->args.value());
                            }
                        } else {
                            if (iargs.size() == 0U) {
                                iargs.push_back(term_call->args.value());
                            }
                        }
                    }
                    if (!iargs.empty()) {
                        if (iargs.size() != st.value().fields.size()) {
                            gen.GeneratorError(term_call->def, "except " + GString(std::to_string(st.value().fields.size()).c_str()) + " args\nNOTE: but got " + GString(std::to_string(iargs.size()).c_str()) + "\nNOTE: if you don't want initialize all fields dont provide any arguments");
                        }
                        eax_break = true;
                        gen.m_builder.mov(gen.reg(Reg::EDX), gen.mem(MemRef::sym("tmp_p")));
                        gen.m_builder.add(gen.reg(Reg::EDX), gen.imm(4));
                        gen.m_builder.mov(gen.mem(MemRef::sym("tmp_p")), gen.reg(Reg::EDX));
                        gen.m_builder.mov(
                            gen.mem(MemRef::baseSym(Reg::EDX, "tmp_stor")),
                            gen.reg(Reg::EAX)
                        );

                        Struct _st = st.value();
                        GMap<GString, DataType> temps;
                        if (_st.temp) {
                            size_t counter{ 0 };
                            for (auto&& el : _st.temps) {
                                DataType targ = term_call->targs[counter++];
                                gen.substitute_template(targ);
                                targ = gen.canonical_type(targ);
                                temps[el] = targ;
                            }
                        }
                        for (int i = 0; i < static_cast<int>(iargs.size()); ++i) {
                            DataType itype = gen.type_of_expr(iargs[i]);
                            DataType ftype = _st.__fields[i].second;
                            if (_st.temp)
                                gen.substitute_template_wct(ftype, temps);
                            if (itype != ftype) {
                                gen.GeneratorError(term_call->def, "missmatch in initializers types for field nth `" + GString(std::to_string(i + 1).c_str()) + "`\nNOTE: field name - `" + _st.__fields[i].first + "`" + "\nNOTE: excepted " + ftype.to_string() + "\nNOTE: but got " + itype.to_string());
                            }
                            gen.gen_expr(iargs[i]);
                            gen.pop_reg(Reg::ECX);
                            gen.m_builder.mov(gen.reg(Reg::EBX), gen.mem(MemRef::sym("tmp_p")));
                            gen.m_builder.mov(gen.reg(Reg::EDX), gen.mem(MemRef::baseSym(Reg::EBX, "tmp_stor")));
                            gen.m_builder.mov(
                                gen.mem(MemRef::baseDisp(Reg::EDX, i * 4)),
                                gen.reg(Reg::ECX)
                            );
                        }
                    }
                    if (!eax_break) {
                        gen.push_reg(Reg::EAX);
                    } else {
                        gen.m_builder.mov(gen.reg(Reg::EDX), gen.mem(MemRef::sym("tmp_p")));
                        gen.push_mem(MemRef::baseSym(Reg::EDX, "tmp_stor"));
                        gen.m_builder.sub(gen.reg(Reg::EDX), gen.imm(4));
                        gen.m_builder.mov(gen.mem(MemRef::sym("tmp_p")), gen.reg(Reg::EDX));
                    }
                    return;
                }
                gen.GeneratorError(term_call->def, "unkown procedure `" + name + "`");
            }

            void operator()(const NodeTermMtCall* term_call) const {
                DataType tpof = gen.type_of_expr(term_call->mt);
                if (!tpof.root().is_object || tpof.root().link)
                    gen.GeneratorError(term_call->def, "can't call method from type " + tpof.to_string() + ".");
                NodeTermNmCall nmcall;
                nmcall.def = term_call->def;
                nmcall.nm = tpof.root().getobjectname();
                nmcall.name = term_call->name;
                nmcall.args = term_call->args;
                nmcall.targs = term_call->targs;
                NodeTerm term;
                term.var = &nmcall;
                gen.gen_term(&term);
            }
            void operator()(const NodeTermNmIdent* nm_ident) const {
                std::optional<Namespace*> nms = gen.namespace_lookup(nm_ident->nm);
                if (!nms.has_value()) {
                    gen.GeneratorError(nm_ident->def, "unkown namespace `" + nm_ident->nm + "`");
                }
                
                Namespace* ns = nms.value();
                auto it = ns->consts.find(nm_ident->name);
                
                if (it != ns->consts.end()) {
                    gen.push_imm(it->second);
                    return;
                }

                // TODO: global variables in namespaces
                
                gen.GeneratorError(nm_ident->def, "unkown member `" + nm_ident->name + "` in namespace `" + nm_ident->nm + "`");
            }
        };

        TermVisitor visitor{ *this, lvalue };
        std::visit(visitor, term->var);
    }

    void gen_bin_expr(const NodeBinExpr* bin_expr, bool lvalue = false)
    {
        struct BinExprVisitor {
            Generator& gen;
            bool       lvalue;
            const NodeBinExpr* base;

            void operator()(const NodeBinExprSub* sub) const {
                DataType oneT = gen.type_of_expr(sub->lhs);
                DataType twoT = gen.type_of_expr(sub->rhs);
                if (oneT.root().is_object) {
                    NodeTermMtCall sb;
                    sb.def = base->def;
                    sb.mt = sub->lhs;
                    sb.name = "m_sub";
                    GVector<NodeExpr*> args;
                    args.push_back(sub->lhs);
                    args.push_back(sub->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = base->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    sb.args = &eargs;
                    NodeTerm asterm{ .var = &sb };
                    NodeExpr asexpr{ .var = &asterm };
                    gen.gen_expr(&asexpr);
                    return;
                }
                if (oneT != twoT &&
                    !(oneT.root() == BaseDataTypePtr && twoT.root() == BaseDataTypeInt) &&
                    !(oneT.root().ptrlvl != 0ULL && twoT.root() == BaseDataTypeInt)) {
                    gen.GeneratorError(base->def, "can't use operator - for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
                }
                gen.gen_expr(sub->rhs);
                gen.gen_expr(sub->lhs);
                gen.pop_reg(Reg::EAX);
                gen.pop_reg(Reg::EBX);
                gen.m_builder.sub(gen.reg(Reg::EAX), gen.reg(Reg::EBX));
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprAdd* add) const {
                DataType oneT = gen.type_of_expr(add->lhs);
                DataType twoT = gen.type_of_expr(add->rhs);
                if (oneT.root().is_object) {
                    NodeTermMtCall dd;
                    dd.def = base->def;
                    dd.mt = add->lhs;
                    dd.name = "m_add";
                    GVector<NodeExpr*> args;
                    args.push_back(add->lhs);
                    args.push_back(add->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = base->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    dd.args = &eargs;
                    NodeTerm asterm{ .var = &dd };
                    NodeExpr asexpr{ .var = &asterm };
                    gen.gen_expr(&asexpr);
                    return;
                }
                if (oneT != twoT &&
                    !(oneT.root() == BaseDataTypePtr && twoT.root() == BaseDataTypeInt) &&
                    !(oneT.root().ptrlvl != 0ULL && twoT.root() == BaseDataTypeInt)) {
                    gen.GeneratorError(base->def, "can't use operator + for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
                }
                gen.gen_expr(add->rhs);
                gen.gen_expr(add->lhs);
                gen.pop_reg(Reg::EAX);
                gen.pop_reg(Reg::EBX);
                gen.m_builder.add(gen.reg(Reg::EAX), gen.reg(Reg::EBX));
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprMulti* multi) const {
                DataType oneT = gen.type_of_expr(multi->lhs);
                DataType twoT = gen.type_of_expr(multi->rhs);
                if (oneT.root().is_object) {
                    NodeTermMtCall dd;
                    dd.def = base->def;
                    dd.mt = multi->lhs;
                    dd.name = "m_mul";
                    GVector<NodeExpr*> args;
                    args.push_back(multi->lhs);
                    args.push_back(multi->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = base->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    dd.args = &eargs;
                    NodeTerm asterm{ .var = &dd };
                    NodeExpr asexpr{ .var = &asterm };
                    gen.gen_expr(&asexpr);
                    return;
                }
                if (oneT != twoT)
                    gen.GeneratorError(base->def, "can't use operator * for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
                gen.gen_expr(multi->rhs);
                gen.gen_expr(multi->lhs);
                gen.pop_reg(Reg::EAX);
                gen.pop_reg(Reg::EBX);
                gen.m_builder.emit(IRInstr(IROp::Mul, gen.reg(Reg::EBX)));
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprDiv* div) const {
                DataType oneT = gen.type_of_expr(div->lhs);
                DataType twoT = gen.type_of_expr(div->rhs);
                if (oneT.root().is_object) {
                    NodeTermMtCall dd;
                    dd.def = base->def;
                    dd.mt = div->lhs;
                    dd.name = "m_div";
                    GVector<NodeExpr*> args;
                    args.push_back(div->lhs);
                    args.push_back(div->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = base->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    dd.args = &eargs;
                    NodeTerm asterm{ .var = &dd };
                    NodeExpr asexpr{ .var = &asterm };
                    gen.gen_expr(&asexpr);
                    return;
                }
                if (oneT != twoT)
                    gen.GeneratorError(base->def, "can't use operator / for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
                gen.gen_expr(div->rhs);
                gen.gen_expr(div->lhs);
                gen.pop_reg(Reg::EAX);
                gen.pop_reg(Reg::EBX);
                gen.m_builder.emit(IRInstr(IROp::Xor, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.emit(IRInstr(IROp::Div, gen.reg(Reg::EBX)));
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprShl* shl) const {
                DataType oneT = gen.type_of_expr(shl->lhs);
                DataType twoT = gen.type_of_expr(shl->rhs);
                if (oneT.root().is_object) {
                    NodeTermMtCall dd;
                    dd.def = base->def;
                    dd.mt = shl->lhs;
                    dd.name = "m_shl";
                    GVector<NodeExpr*> args;
                    args.push_back(shl->lhs);
                    args.push_back(shl->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = base->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    dd.args = &eargs;
                    NodeTerm asterm{ .var = &dd };
                    NodeExpr asexpr{ .var = &asterm };
                    gen.gen_expr(&asexpr);
                    return;
                }
                if (oneT != twoT)
                    gen.GeneratorError(base->def, "can't use operator << for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
                gen.gen_expr(shl->rhs);
                gen.gen_expr(shl->lhs);
                gen.pop_reg(Reg::EAX);
                gen.pop_reg(Reg::ECX);
                gen.m_builder.emit(IRInstr(IROp::Shl, gen.reg(Reg::EAX), gen.reg(Reg::CL)));
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprShr* shr) const {
                DataType oneT = gen.type_of_expr(shr->lhs);
                DataType twoT = gen.type_of_expr(shr->rhs);
                if (oneT.root().is_object) {
                    NodeTermMtCall dd;
                    dd.def = base->def;
                    dd.mt = shr->lhs;
                    dd.name = "m_shr";
                    GVector<NodeExpr*> args;
                    args.push_back(shr->lhs);
                    args.push_back(shr->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = base->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    dd.args = &eargs;
                    NodeTerm asterm{ .var = &dd };
                    NodeExpr asexpr{ .var = &asterm };
                    gen.gen_expr(&asexpr);
                    return;
                }
                if (oneT != twoT)
                    gen.GeneratorError(base->def, "can't use operator >> for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
                gen.gen_expr(shr->rhs);
                gen.gen_expr(shr->lhs);
                gen.pop_reg(Reg::EAX);
                gen.pop_reg(Reg::ECX);
                gen.m_builder.emit(IRInstr(IROp::Shr, gen.reg(Reg::EAX), gen.reg(Reg::CL)));
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprMod* md) const {
                DataType oneT = gen.type_of_expr(md->lhs);
                DataType twoT = gen.type_of_expr(md->rhs);
                if (oneT.root().is_object) {
                    NodeTermNmCall m;
                    m.def = base->def;
                    m.nm = "std";
                    m.name = "mod";
                    GVector<NodeExpr*> args;
                    args.push_back(md->lhs);
                    args.push_back(md->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = base->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    m.args = &eargs;
                    NodeTerm asterm{ .var = &m };
                    NodeExpr asexpr{ .var = &asterm };
                    gen.gen_expr(&asexpr);
                    return;
                }
                if (oneT != twoT)
                    gen.GeneratorError(base->def, "can't use operator % for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
                gen.gen_expr(md->rhs);
                gen.gen_expr(md->lhs);
                gen.pop_reg(Reg::EAX);
                gen.pop_reg(Reg::EBX);
                gen.m_builder.emit(IRInstr(IROp::Xor, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.emit(IRInstr(IROp::Div, gen.reg(Reg::EBX)));
                gen.push_reg(Reg::EDX);
            }

            void operator()(const NodeBinExprEqEq* eqeq) const {
                DataType oneT = gen.type_of_expr(eqeq->lhs);
                DataType twoT = gen.type_of_expr(eqeq->rhs);
                if (oneT.root().is_object) {
                    NodeTermMtCall equal;
                    equal.def = base->def;
                    equal.mt = eqeq->lhs;
                    equal.name = "m_equal";
                    GVector<NodeExpr*> args;
                    args.push_back(eqeq->lhs);
                    args.push_back(eqeq->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = base->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    equal.args = &eargs;
                    NodeTerm asterm{ .var = &equal };
                    NodeExpr asexpr{ .var = &asterm };
                    gen.gen_expr(&asexpr);
                    return;
                }
                if (oneT != twoT)
                    gen.GeneratorError(base->def, "can't use operator == for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
                gen.gen_expr(eqeq->rhs);
                gen.gen_expr(eqeq->lhs);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.mov(gen.reg(Reg::EDX), gen.imm(0));
                gen.m_builder.mov(gen.reg(Reg::ECX), gen.imm(1));
                gen.m_builder.emit(IRInstr(IROp::Cmp, gen.reg(Reg::EAX), gen.reg(Reg::EBX)));
                gen.m_builder.emit(IRInstr(IROp::CMovE, gen.reg(Reg::EDX), gen.reg(Reg::ECX)));
                gen.push_reg(Reg::EDX);
            }

            void operator()(const NodeBinExprNotEq* nq) const {
                DataType oneT = gen.type_of_expr(nq->lhs);
                DataType twoT = gen.type_of_expr(nq->rhs);
                if (oneT.root().is_object) {
                    NodeTermMtCall nequal;
                    nequal.def = base->def;
                    nequal.mt = nq->lhs;
                    nequal.name = "m_not_equal";
                    GVector<NodeExpr*> args;
                    args.push_back(nq->lhs);
                    args.push_back(nq->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = base->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    nequal.args = &eargs;
                    NodeTerm asterm{ .var = &nequal };
                    NodeExpr asexpr{ .var = &asterm };
                    gen.gen_expr(&asexpr);
                    return;
                }
                if (oneT != twoT)
                    gen.GeneratorError(base->def, "can't use operator != for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
                gen.gen_expr(nq->rhs);
                gen.gen_expr(nq->lhs);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.mov(gen.reg(Reg::EDX), gen.imm(0));
                gen.m_builder.mov(gen.reg(Reg::ECX), gen.imm(1));
                gen.m_builder.emit(IRInstr(IROp::Cmp, gen.reg(Reg::EAX), gen.reg(Reg::EBX)));
                gen.m_builder.emit(IRInstr(IROp::CMovNE, gen.reg(Reg::EDX), gen.reg(Reg::ECX)));
                gen.push_reg(Reg::EDX);
            }

            void operator()(const NodeBinExprLess* less) const {
                DataType oneT = gen.type_of_expr(less->lhs);
                DataType twoT = gen.type_of_expr(less->rhs);
                if (oneT.root().is_object) {
                    NodeTermMtCall mtless;
                    mtless.def = base->def;
                    mtless.mt = less->lhs;
                    mtless.name = "m_less";
                    GVector<NodeExpr*> args;
                    args.push_back(less->lhs);
                    args.push_back(less->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = base->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    mtless.args = &eargs;
                    NodeTerm asterm{ .var = &mtless };
                    NodeExpr asexpr{ .var = &asterm };
                    gen.gen_expr(&asexpr);
                    return;
                }
                if (oneT != twoT)
                    gen.GeneratorError(base->def, "can't use operator < for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
                gen.gen_expr(less->lhs);
                gen.gen_expr(less->rhs);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.mov(gen.reg(Reg::EDX), gen.imm(0));
                gen.m_builder.mov(gen.reg(Reg::ECX), gen.imm(1));
                gen.m_builder.emit(IRInstr(IROp::Cmp, gen.reg(Reg::EAX), gen.reg(Reg::EBX)));
                gen.m_builder.emit(IRInstr(IROp::CMovL, gen.reg(Reg::EDX), gen.reg(Reg::ECX)));
                gen.push_reg(Reg::EDX);
            }

            void operator()(const NodeBinExprAnd* band) const {
                gen.gen_expr(band->lhs);
                GString flab = gen.create_label();
                GString elab = gen.create_label();
                gen.pop_reg(Reg::EDX);
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.jz(gen.label(flab));
                gen.gen_expr(band->rhs);
                gen.pop_reg(Reg::EDX);
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.jz(gen.label(flab));
                gen.m_builder.mov(gen.reg(Reg::EAX), gen.imm(1));
                gen.m_builder.jmp(gen.label(elab));
                gen.m_builder.label(flab);
                gen.m_builder.mov(gen.reg(Reg::EAX), gen.imm(0));
                gen.m_builder.label(elab);
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprOr* bor) const {
                GString flab = gen.create_label();
                GString elab = gen.create_label();
                GString tlab = gen.create_label();
                gen.gen_expr(bor->lhs);
                gen.pop_reg(Reg::EDX);
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.jnz(gen.label(tlab));
                gen.gen_expr(bor->rhs);
                gen.pop_reg(Reg::EDX);
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.jz(gen.label(flab));
                gen.m_builder.label(tlab);
                gen.m_builder.mov(gen.reg(Reg::EAX), gen.imm(1));
                gen.m_builder.jmp(gen.label(elab));
                gen.m_builder.label(flab);
                gen.m_builder.mov(gen.reg(Reg::EAX), gen.imm(0));
                gen.m_builder.label(elab);
                gen.push_reg(Reg::EAX);
            }

            void operator()(const NodeBinExprAbove* above) const {
                DataType oneT = gen.type_of_expr(above->lhs);
                DataType twoT = gen.type_of_expr(above->rhs);
                if (oneT.root().is_object) {
                    NodeTermMtCall mtabove;
                    mtabove.def = base->def;
                    mtabove.mt = above->lhs;
                    mtabove.name = "m_above";
                    GVector<NodeExpr*> args;
                    args.push_back(above->lhs);
                    args.push_back(above->rhs);
                    NodeBinExprArgs bargs{ .args = args };
                    NodeBinExpr bexpr{ .def = base->def, .var = &bargs };
                    NodeExpr eargs{ .var = &bexpr };
                    mtabove.args = &eargs;
                    NodeTerm asterm{ .var = &mtabove };
                    NodeExpr asexpr{ .var = &asterm };
                    gen.gen_expr(&asexpr);
                    return;
                }
                if (oneT != twoT)
                    gen.GeneratorError(base->def, "can't use operator > for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
                gen.gen_expr(above->lhs);
                gen.gen_expr(above->rhs);
                gen.pop_reg(Reg::EBX);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.mov(gen.reg(Reg::EDX), gen.imm(0));
                gen.m_builder.mov(gen.reg(Reg::ECX), gen.imm(1));
                gen.m_builder.emit(IRInstr(IROp::Cmp, gen.reg(Reg::EAX), gen.reg(Reg::EBX)));
                gen.m_builder.emit(IRInstr(IROp::CMovG, gen.reg(Reg::EDX), gen.reg(Reg::ECX)));
                gen.push_reg(Reg::EDX);
            }

            void operator()(const NodeBinExprDot* dot) const {
                DataType otype = gen.type_of_expr(dot->lhs);

                if (!std::holds_alternative<NodeTerm*>(dot->rhs->var)) {
                    gen.GeneratorError(base->def, "after `.` except identificator");
                }

                if (gen.is_interface_type(otype)) {
                    NodeTerm* rhs_term = std::get<NodeTerm*>(dot->rhs->var);
                    if (!std::holds_alternative<NodeTermCall*>(rhs_term->var)) {
                        gen.GeneratorError(base->def, "dynamic interface call must be method call");
                    }
                    NodeTermCall* call = std::get<NodeTermCall*>(rhs_term->var);
                    GString mname = call->name;

                    Interface iface = gen.get_interface(otype, base->def);

                    int idx = -1;
                    for (int i = 0; i < static_cast<int>(iface.method_order.size()); ++i) {
                        if (iface.method_order[i] == mname) { idx = i; break; }
                    }
                    if (idx < 0) {
                        gen.GeneratorError(base->def,
                            "interface `" + iface.name + "` has no method `" + mname + "`");
                    }

                    gen.gen_expr(dot->lhs);
                    gen.pop_reg(Reg::ECX); 

                    gen.m_builder.mov(gen.reg(Reg::EDX),
                                      gen.mem(MemRef::baseDisp(Reg::ECX, 0)));
                    gen.m_builder.mov(gen.reg(Reg::EAX),
                                      gen.mem(MemRef::baseDisp(Reg::ECX, (1 + idx) * 4)));

                    GVector<NodeExpr*> args_raw;
                    if (call->args.has_value()) {
                        args_raw = gen.__getargs(call->args.value());
                    }

                    const auto mit = iface.methods.find(mname);
                    if (mit == iface.methods.end()) {
                        gen.GeneratorError(base->def,
                            "interface `" + iface.name + "` has no method `" + mname + "`");
                    }
                    const InterfaceMethodInfo& im = mit->second;

                    size_t total_params      = im.params.size();
                    size_t explicit_expected = (total_params > 0 ? total_params - 1 : 0);
                    size_t explicit_got      = args_raw.size();

                    if (explicit_got != explicit_expected) {
                        gen.GeneratorError(call->def,
                            "method `" + mname + "` of interface `" + iface.name +
                            "` expects " + GString(std::to_string(explicit_expected).c_str()) +
                            " argument(s), but got " + GString(std::to_string(explicit_got)).c_str());
                    }

                    for (size_t i = 0; i < explicit_expected; ++i) {
                        DataType formal = gen.canonical_type(im.params[i + 1].second);
                        DataType actual = gen.canonical_type(gen.type_of_expr(args_raw[i]));

                        if (formal.is_object() && formal.getobjectname() == "self") {
                            if (!actual.is_object() || actual.getobjectname() != iface.name) {
                                gen.GeneratorError(call->def,
                                    "argument " + GString(std::to_string(i + 1).c_str()) + " of method `" + mname +
                                    "` of interface `" + iface.name +
                                    "` must be of interface type `" + iface.name +
                                    "`, but got " + actual.to_string());
                            }
                        } else {
                            if (formal != actual) {
                                gen.GeneratorError(call->def,
                                    "argument " + GString(std::to_string(i + 1).c_str()) +
                                    " of method `" + mname + "` of interface `" + iface.name +
                                    "` expects " + formal.to_string() +
                                    ", but got " + actual.to_string());
                            }
                        }
                    }

                    for (int i = static_cast<int>(args_raw.size()) - 1; i >= 0; --i) {
                        gen.gen_expr(args_raw[i]);
                    }
                    gen.push_reg(Reg::EDX); 

                    gen.m_builder.emit(IRInstr(IROp::InlineAsm,
                                               Operand::symbolOp("call eax")));

                    size_t pop_bytes = (1 + args_raw.size()) * 4;
                    if (pop_bytes != 0) {
                        gen.m_builder.add(gen.reg(Reg::ESP),
                                          gen.imm(static_cast<int>(pop_bytes)));
                    }

                    DataType ret = iface.methods[mname].rettype;
                    if (ret != BaseDataTypeVoid && !lvalue) {
                        gen.push_reg(Reg::EAX);
                    }
                    return;
                }

                NodeTerm* term = std::get<NodeTerm*>(dot->rhs->var);

                if (std::holds_alternative<NodeTermCall*>(term->var)) {
                    NodeTermCall* call = std::get<NodeTermCall*>(term->var);

                    NodeTermMtCall mt;
                    mt.def   = call->def;
                    mt.mt    = dot->lhs;
                    mt.name  = call->name;
                    mt.targs = call->targs;

                    GVector<NodeExpr*> allargs;
                    allargs.push_back(dot->lhs);

                    if (call->args.has_value()) {
                        NodeExpr* aexpr = call->args.value();
                        if (std::holds_alternative<NodeBinExpr*>(aexpr->var) &&
                            std::holds_alternative<NodeBinExprArgs*>(std::get<NodeBinExpr*>(aexpr->var)->var))
                        {
                            NodeBinExpr* be = std::get<NodeBinExpr*>(aexpr->var);
                            NodeBinExprArgs* barg = std::get<NodeBinExprArgs*>(be->var);
                            auto& raw = barg->args;
                            allargs.insert(allargs.end(), raw.begin(), raw.end());
                        } else {
                            allargs.push_back(aexpr);
                        }
                    }

                    NodeBinExprArgs bargs{ .args = allargs };
                    NodeBinExpr     bexpr{ .def = call->def, .var = &bargs };
                    NodeExpr        eargs{ .var = &bexpr };

                    mt.args = &eargs;

                    NodeTerm wrapTerm{ .var = &mt };
                    gen.gen_term(&wrapTerm);
                    return;
                }

                if (!std::holds_alternative<NodeTermIdent*>(term->var)) {
                    gen.GeneratorError(base->def, "after `.` except identificator");
                }

                NodeTermIdent* tid = std::get<NodeTermIdent*>(term->var);
                Token ident = tid->ident;
                GString field_name = ident.value.value();

                if (!otype.root().is_object) {
                    gen.GeneratorError(
                        base->def,
                        "bellow `.` except expression of type any object\nNOTE: but got " +
                        otype.to_string()
                    );
                }

                GString struct_name = otype.root().getobjectname();
                std::optional<Struct> st = gen.struct_lookup(struct_name);
                size_t field_offset = 0;
                if (st.has_value()) {
                    std::optional<Field> field = gen.field_lookup(st.value(), field_name);
                    if (!field.has_value()) {
                        gen.GeneratorError(
                            base->def,
                            "object of type `" + otype.to_string() +
                            "` doesn`t have field `" + field_name + "`"
                        );
                    }
                    field_offset = field.value().nth;
                } else {
                    std::optional<Interface> inter = gen.inter_lookup(struct_name);
                    if (inter.has_value()) {
                        gen.GeneratorError(
                            base->def,
                            "interfaces have no fields; cannot access `" + field_name +
                            "` on interface `" + struct_name + "`"
                        );
                    } else {
                        assert(false && "unreacheable");
                    }
                }

                gen.gen_expr(dot->lhs);
                gen.pop_reg(Reg::ECX);
                int32_t disp = static_cast<int32_t>(field_offset * 4U);
                if (lvalue) {
                    if (disp != 0)
                        gen.m_builder.add(gen.reg(Reg::ECX), gen.imm(disp));
                    gen.push_reg(Reg::ECX);
                } else {
                    MemRef m = MemRef::baseDisp(Reg::ECX, disp);
                    gen.push_mem(m);
                }
            }

            void operator()(const NodeBinExprArgs* args) const {
                for (int i = static_cast<int>(args->args.size()) - 1; i > -1; --i) {
                    gen.gen_expr(args->args[i]);
                }
            }
            void operator()(const NodeBinExprIndex* idx) const {
                NodeTermMtCall call;
                call.def = base->def;
                call.mt = idx->lhs;
                if (lvalue) {
                    call.name = "m_index_ref";
                } else {
                    call.name = "m_index";
                }
                
                GVector<NodeExpr*> args_list;
                args_list.push_back(idx->lhs);
                args_list.push_back(idx->rhs);
                NodeBinExprArgs bargs; bargs.args = args_list;
                NodeBinExpr be; be.var = &bargs;
                NodeExpr ae; ae.var = &be;
                
                call.args = &ae;
                
                NodeTerm t; t.var = &call;
                
                gen.gen_term(&t);
            }
        };

        BinExprVisitor visitor{ *this, lvalue, bin_expr };
        std::visit(visitor, bin_expr->var);
    }

    void gen_expr(const NodeExpr* expr, bool lvalue = false)
    {
        struct ExprVisitor {
            Generator& gen;
            bool       lvalue;

            void operator()(const NodeTerm* term) const
            {
                gen.gen_term(term, lvalue);
            }

            void operator()(const NodeBinExpr* bin_expr) const
            {
                gen.gen_bin_expr(bin_expr, lvalue);
            }
        };

        ExprVisitor visitor{ *this, lvalue };
        std::visit(visitor, expr->var);
    }

    size_t collect_alligns(const NodeScope* scope) noexcept {
        size_t fsz = 0ULL;
        for (const NodeStmt* stmt : scope->stmts) {
            if (std::holds_alternative<NodeStmtLet*>(stmt->var)) {
                fsz += 1;
            } else if (std::holds_alternative<NodeStmtCompileTimeIf*>(stmt->var)) {
                NodeStmtCompileTimeIf* ctif = std::get<NodeStmtCompileTimeIf*>(stmt->var);
                fsz += collect_alligns(ctif->_if);
                if (ctif->_else.has_value()) {
                    fsz += collect_alligns(ctif->_else.value());
                }
            } else if (std::holds_alternative<NodeStmtBuffer*>(stmt->var)) {
                fsz += eval(std::get<NodeStmtBuffer*>(stmt->var)->size,
                            std::get<NodeStmtBuffer*>(stmt->var)->def) /
                       4ULL;
            }
        }
        return fsz;
    }

    void gen_scope(const NodeScope* scope, size_t psize = 0, bool wbreak = false)
    {
        size_t sz = psize + collect_alligns(scope);
        if (wbreak) {
            m_break_scopes.push_back(sz);
        }
        begin_scope(static_cast<int>(sz));
        for (const NodeStmt* stmt : scope->stmts) {
            gen_stmt(stmt);
        }
        end_scope();
    }

    void gen_scope_sp(const NodeScope* scope, const NodeStmtProc* proc, const Procedure& __proc)
    {
        size_t loc_sz = proc->params.size() + collect_alligns(scope);
        begin_scope(static_cast<int>(loc_sz));

        if (std::find(proc->attrs.begin(), proc->attrs.end(), ProcAttr::nostdargs) ==
            proc->attrs.end())
        {
            for (int i = 0; i < static_cast<int>(proc->params.size()); ++i) {
                create_var_va(__proc.params[i].first, __proc.params[i].second, proc->def);

                m_builder.mov(
                    reg(Reg::EDX),
                    mem(MemRef::baseDisp(Reg::EBP, i * 4 + 8))
                );
                m_builder.mov(
                    mem(MemRef::baseDisp(Reg::EBP, -(i * 4 + 4))),
                    reg(Reg::EDX)
                );
            }
        }

        for (const NodeStmt* stmt : scope->stmts) {
            gen_stmt(stmt);
        }
        end_scope_sp(m_cur_proc.value(), proc->def);
    }

    void convert(NodeExpr* expr, const DataType& to, const DataType& from, const Token& def) {
        if (to.root().is_object) {
            GString objName = to.root().getobjectname();
            std::optional<Namespace*> _nms = namespace_lookup(objName);
            if (!_nms.has_value()) goto CONVERT_FAILED;
            NodeTermNmCall call;
            call.def = def;
            call.nm = objName;
            call.name = "new";
            GVector<NodeExpr*> args;
            args.push_back(expr);
            NodeBinExprArgs bargs{ .args = args };
            NodeBinExpr     bexpr{ .def = def, .var = &bargs };
            NodeExpr        eargs{ .var = &bexpr };
            call.args = &eargs;
            NodeTerm at;
            at.var = &call;
            gen_term(&at);
            return;
        }
CONVERT_FAILED:
        GeneratorError(def, "type " + from.to_string() + " is not convertible to " + to.to_string() + ".");
    }

    void create_var(__str_ref name, NodeExpr* value, const Token& where, std::optional<DataType> vtype) {
        if (m_scopes_vi.size() == 0ULL) {
            GeneratorError(where, "can't create global variable with assignment");
        }
        std::optional<Var> ivar = var_lookup_cs(name);
        if (ivar.has_value()) {
            GeneratorError(where, "name `" + name + "` already in use");
        }
        DataType rtype;
        DataType got_t = type_of_expr(value);
        bool lval = false;
        if (!vtype.has_value()) rtype = got_t;
        else {
            DataType got_t2 = type_of_expr(value);
            rtype = vtype.value();
            substitute_template(rtype);
            if (rtype.is_object() && is_interface_type(rtype)) {
                ensure_implements_interface(got_t, rtype, where);
                gen_expr(value);
                gen_box_interface(rtype, got_t, where);
                goto AFTER_GEN;
            }
            if (rtype != got_t2 && !rtype.root().is_object) {
                if (rtype.root().link && !got_t2.root().link) {
                    lval = true;
                } else {
                    GeneratorError(where, "let except type " + rtype.to_string() + ", but got " + got_t2.to_string() + ".");
                }
            } else if (rtype != got_t2 && rtype.root().is_object) {
                convert(value, rtype, got_t2, where);
                goto AFTER_GEN;
            }
        }
        if (got_t.root().rvalue)
            GeneratorError(where, "assigning rvalue-reference to variable.");
        gen_expr(value, lval);
AFTER_GEN:
        last_scope()[name] = { .name = name, .stack_loc = ++m_var_index * 4, .type = rtype };
        pop_reg(Reg::ECX);
        m_builder.mov(mem(local_mem(m_var_index * 4)), reg(Reg::ECX));
    }

    void create_var_va(__str_ref name, const DataType& type, const Token& where) {
        if (m_scopes_vi.size() == 0ULL) {
            std::optional<GVar> ivar = gvar_lookup(name);
            if (ivar.has_value()) {
                GeneratorError(where, "name `" + name + "` already in use");
            }
            m_global_vars[name] = { .name = name, .type = type };
            return;
        }
        std::optional<Var> ivar = var_lookup_cs(name);
        if (ivar.has_value()) {
            GeneratorError(where, "name `" + name + "` already in use");
        }
        last_scope()[name] = { .name = name, .stack_loc = ++m_var_index * 4, .type = type };
    }

    void create_var_va_wid(__str_ref name, const DataType& type, const Token& where) {
        std::optional<Var> ivar = var_lookup_cs(name);
        if (ivar.has_value()) {
            GeneratorError(where, "name `" + name + "` already in use");
        }
        last_scope()[name] = { .name = name, .stack_loc = m_var_index * 4, .type = type };
    }

    void gen_if_pred(const NodeIfPred* pred, __str_ref end_label)
    {
        struct PredVisitor {
            Generator& gen;
            __str_ref  end_label;

            void operator()(const NodeIfPredElif* elif) const
            {
                gen.gen_expr(elif->expr);
                gen.pop_reg(Reg::EAX);
                const GString label = gen.create_label();
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EAX), gen.reg(Reg::EAX)));
                gen.m_builder.jz(gen.label(label));
                gen.gen_scope(elif->scope);
                gen.m_builder.jmp(gen.label(end_label));
                gen.m_builder.label(label);
                if (elif->pred.has_value()) {
                    gen.gen_if_pred(elif->pred.value(), end_label);
                }
            }

            void operator()(const NodeIfPredElse* else_) const
            {
                gen.gen_scope(else_->scope);
            }
        };

        PredVisitor visitor{ *this, end_label };
        std::visit(visitor, pred->var);
    }

    Procedure __proc_get(__str_ref name, const Token& def) {
        std::optional<Procedure> proc = proc_lookup(name);
        if (!proc.has_value()) {
            GeneratorError(def, "unkown procedure `" + name + "`");
        }
        return proc.value();
    }

    void __typecheck_call(const GVector<NodeExpr*>& args,
                          const GVector<std::pair<GString, DataType>>& params,
                          const Token& def,
                          const Procedure& proc,
                          size_t* stack_allign)
    {
        bool nosizedargs =
            std::find(proc.attrs.begin(), proc.attrs.end(), ProcAttr::nosizedargs) != proc.attrs.end();

        const size_t args_sz   = args.size();
        const size_t params_sz = params.size();

        if (!nosizedargs) {
            if (args_sz != params_sz) {
                GeneratorError(
                    def,
                    "procedure `" + proc.name + "` expects " +
                    GString(std::to_string(params_sz).c_str()) + " argument(s), but " +
                    GString(std::to_string(args_sz).c_str()) + " were provided"
                );
            }
        } else {
            if (args_sz < params_sz) {
                GeneratorError(
                    def,
                    "procedure `" + proc.name + "` expects at least " +
                    GString(std::to_string(params_sz).c_str()) + " argument(s), but " +
                    GString(std::to_string(args_sz).c_str()) + " were provided"
                );
            }
        }

        int bad = -1;
        if (!match_call_signature(args, params, proc, nosizedargs, &bad)) {
            if (bad < 0 || bad >= static_cast<int>(params_sz) || bad >= static_cast<int>(args_sz)) {
                GeneratorError(
                    def,
                    "procedure `" + proc.name + "` has incompatible argument types"
                );
            } else {
                GeneratorError(
                    def,
                    "procedure `" + proc.name + "`\nexcept type " +
                    canonical_type(params[bad].second).to_string() + " at " +
                    GString(std::to_string(bad).c_str()) + " argument\nNOTE: but found type " +
                    canonical_type(type_of_expr(args[bad])).to_string()
                );
            }
        }

        *stack_allign += args_sz;
    }

    void substitute_template_wct(DataType& type, GMap<GString, DataType>& temps) {
        bool generics_changed = false;
        GVector<DataType> new_generics;
        
        if (!type.node->generics.empty()) {
            new_generics = type.node->generics;
            for(auto& arg : new_generics) {
                DataType old_arg = arg;
                substitute_template_wct(arg, temps);
                if (arg != old_arg) generics_changed = true;
            }
        }

        if (type.root().is_object) {
            GString oname = type.root().getobjectname();
            const auto it = temps.find(oname);
            
            if (it != temps.end()) {
                DataType mapped = it->second;
                
                DataType res(mapped.root());
                res.node->generics = mapped.node->generics;
                
                res.root().ptrlvl += type.root().ptrlvl;
                if (type.root().link)   res.root().link   = true;
                if (type.root().rvalue) res.root().rvalue = true;
                
                type = res; 
                return; 
            }
        }

        if (generics_changed) {
            DataType res(type.root());
            res.node->generics = new_generics;
            type = res;
        }
    }

    void substitute_template(DataType& type) {
        if (m_temps.empty()) return;
        
        substitute_template_wct(type, m_temps.back());
    }

    bool __try_typecheck_call(const GVector<NodeExpr*>& args, const Procedure& proc) {
        bool nosizedargs =
            std::find(proc.attrs.begin(), proc.attrs.end(), ProcAttr::nosizedargs) != proc.attrs.end();
        return match_call_signature(args, proc.params, proc, nosizedargs, nullptr);
    }

    GVector<NodeExpr*> __getargs(NodeExpr* __expr) {
        return std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(__expr->var)->var)->args;
    }

    std::optional<int> __eval_ctcall(const NodeTermCall* call, const Token& def) {
        __str_ref name = call->name;
        if (name == "is_same_t") {
            if (!call->args.has_value()) {
                GeneratorError(def, "is_same_t excepts 2 args");
            }
            GVector<NodeExpr*> args = __getargs(call->args.value());
            if (args.size() != 2) {
                GeneratorError(def, "is_same_t excepts 2 args");
            }
            std::optional<NodeTermType*> type_1 = ptools::get::type(args[0]);
            std::optional<NodeTermType*> type_2 = ptools::get::type(args[1]);
            DataType one;
            DataType two;
            if (type_1.has_value()) one = type_1.value()->type;
            else                    one = type_of_expr(args[0]);
            if (type_2.has_value()) two = type_2.value()->type;
            else                    two = type_of_expr(args[1]);
            substitute_template(one);
            substitute_template(two);
            return static_cast<int>(one == two);
        }
        if (name == "is_object_t") {
            if (!call->args.has_value()) {
                GeneratorError(def, "is_object_t excepts 1 args");
            }
            GVector<NodeExpr*> args = __getargs(call->args.value());
            if (args.size() != 1) {
                GeneratorError(def, "is_object_t excepts 1 args");
            }
            std::optional<NodeTermType*> type = ptools::get::type(args[0]);
            DataType tp;
            if (type.has_value()) tp = type.value()->type;
            else                  tp = type_of_expr(args[0]);
            substitute_template(tp);
            return static_cast<int>(tp.root().is_object);
        }
        if (name == "ct_not") {
            if (!call->args.has_value()) {
                GeneratorError(def, "ct_not excepts 1 args");
            }
            GVector<NodeExpr*> args = __getargs(call->args.value());
            if (args.size() != 1) {
                GeneratorError(def, "ct_not excepts 1 args");
            }
            return static_cast<int>(!(static_cast<bool>(eval(args[0], def))));
        }
        if (name == "is_implements_t") {
            if (!call->args.has_value()) GeneratorError(def, "is_implements_t excepts 2 args");
            GVector<NodeExpr*> args = __getargs(call->args.value());
            if (args.size() != 2) GeneratorError(def, "is_implements_t excepts 2 args");

            std::optional<NodeTermType*> type_1 = ptools::get::type(args[0]);
            std::optional<NodeTermType*> type_2 = ptools::get::type(args[1]);
            
            DataType ty;
            if (type_1.has_value()) ty = type_1.value()->type;
            else                    ty = type_of_expr(args[0]);
            
            DataType iface;
            if (type_2.has_value()) iface = type_2.value()->type;
            else                    iface = type_of_expr(args[1]);

            substitute_template(ty);
            substitute_template(iface);


            bool result = check_implements_quiet(ty, iface);
            return static_cast<int>(result);
        }
        return std::nullopt;
    }

    int eval(const NodeExpr* expr, const Token& def) {
        int result = 0;
        if (std::holds_alternative<NodeTerm*>(expr->var)) {
            NodeTerm* nterm = std::get<NodeTerm*>(expr->var);
            if (std::holds_alternative<NodeTermIntLit*>(nterm->var)) {
                return std::stoul(std::get<NodeTermIntLit*>(nterm->var)->int_lit.value.value().c_str());
            }
            if (std::holds_alternative<NodeTermCtMdefined*>(nterm->var)) {
                return static_cast<int>(std::get<NodeTermCtMdefined*>(nterm->var)->value);
            }
            if (std::holds_alternative<NodeTermIdent*>(nterm->var)) {
                GString cname = std::get<NodeTermIdent*>(nterm->var)->ident.value.value();
                if (cname == "iota") return CTX_IOTA++;
                if (cname == "reset") {
                    int old = CTX_IOTA;
                    CTX_IOTA = 0;
                    return old;
                }
                if (cname == "true")  return 1;
                if (cname == "false") return 0;
                std::optional<Constant> cns = const_lookup(cname);
                if (cns.has_value()) return cns.value().value;
            }
            if (std::holds_alternative<NodeTermNmIdent*>(nterm->var)) {
                NodeTermNmIdent* nm = std::get<NodeTermNmIdent*>(nterm->var);
                std::optional<Namespace*> nms = namespace_lookup(nm->nm);
                if (nms.has_value()) {
                    auto it = nms.value()->consts.find(nm->name);
                    if (it != nms.value()->consts.end()) return it->second;
                }
                GeneratorError(nm->def, "unkown constant");
            }
            if (std::holds_alternative<NodeTermCall*>(nterm->var)) {
                NodeTermCall* call = std::get<NodeTermCall*>(nterm->var);
                if (auto vl = __eval_ctcall(call, def)) return vl.value();
                GeneratorError(def, "unkown compile-time procedure `" + call->name + "`");
            }
        }
        if (std::holds_alternative<NodeBinExpr*>(expr->var)) {
            NodeBinExpr* nbin = std::get<NodeBinExpr*>(expr->var);
            if (std::holds_alternative<NodeBinExprAdd*>(nbin->var)) {
                NodeBinExprAdd* nadd = std::get<NodeBinExprAdd*>(nbin->var);
                return eval(nadd->lhs, def) + eval(nadd->rhs, def);
            }
            if (std::holds_alternative<NodeBinExprSub*>(nbin->var)) {
                NodeBinExprSub* nsub = std::get<NodeBinExprSub*>(nbin->var);
                return eval(nsub->lhs, def) - eval(nsub->rhs, def);
            }
            if (std::holds_alternative<NodeBinExprMulti*>(nbin->var)) {
                NodeBinExprMulti* nmul = std::get<NodeBinExprMulti*>(nbin->var);
                return eval(nmul->lhs, def) * eval(nmul->rhs, def);
            }
            if (std::holds_alternative<NodeBinExprDiv*>(nbin->var)) {
                NodeBinExprDiv* ndiv = std::get<NodeBinExprDiv*>(nbin->var);
                return eval(ndiv->lhs, def) / eval(ndiv->rhs, def);
            }
            if (std::holds_alternative<NodeBinExprEqEq*>(nbin->var)) {
                NodeBinExprEqEq* neqeq = std::get<NodeBinExprEqEq*>(nbin->var);
                return eval(neqeq->lhs, def) == eval(neqeq->rhs, def);
            }
            if (std::holds_alternative<NodeBinExprNotEq*>(nbin->var)) {
                NodeBinExprNotEq* nnoteq = std::get<NodeBinExprNotEq*>(nbin->var);
                return eval(nnoteq->lhs, def) != eval(nnoteq->rhs, def);
            }
            if (std::holds_alternative<NodeBinExprLess*>(nbin->var)) {
                NodeBinExprLess* nless = std::get<NodeBinExprLess*>(nbin->var);
                return eval(nless->lhs, def) < eval(nless->rhs, def);
            }
            if (std::holds_alternative<NodeBinExprAbove*>(nbin->var)) {
                NodeBinExprAbove* nabove = std::get<NodeBinExprAbove*>(nbin->var);
                return eval(nabove->lhs, def) > eval(nabove->rhs, def);
            }
            if (std::holds_alternative<NodeBinExprAnd*>(nbin->var)) {
                NodeBinExprAnd* nand = std::get<NodeBinExprAnd*>(nbin->var);
                return eval(nand->lhs, def) && eval(nand->rhs, def);
            }
            if (std::holds_alternative<NodeBinExprOr*>(nbin->var)) {
                NodeBinExprOr* nor = std::get<NodeBinExprOr*>(nbin->var);
                return eval(nor->lhs, def) || eval(nor->rhs, def);
            }
        }
        GeneratorError(def, "the expression cannot be evaluated at compile time.");
        return result;
    }

    bool in_namespace() {
        return m_cur_namespace != NULL;
    }

    GMap<GString, DataType> try_derive_templates(GVector<DataType>& targs,
                                                      const GVector<std::pair<GString, DataType>>& params,
                                                      const Token& def,
                                                      GVector<GString>* templates,
                                                      const GVector<NodeExpr*> args,
                                                      const Procedure& proc)
    {
        if (params.empty()) {
            GeneratorError(def, "can't substitute type " + *templates->begin() + ", params empty.");
        }
        if (params.size() != args.size()) {
            GeneratorError(def, "procedure `" + proc.name +
                                 "` excepts " + GString(std::to_string(params.size()).c_str()) +
                                 " args, but got " + GString(std::to_string(args.size()).c_str()) + ".");
        }

        assert(templates != NULL);
        assert(!templates->empty());

        GMap<GString, DataType> temps;
        bool ok = derive_templates_core(targs, params, templates, args, proc, temps);
        if (!ok) {
            int bad = -1;
            for (int i = 0; i < static_cast<int>(templates->size()); ++i) {
                if (targs[i].root().is_simple() &&
                    targs[i].root().getsimpletype() == SimpleDataType::_void) {
                    bad = i;
                    break;
                }
            }
            if (bad >= 0) {
                GeneratorError(def,
                    "failed to substitute template argument `" +
                    templates->operator[](bad) + "`.");
            } else {
                GeneratorError(def, "failed to substitute template arguments.");
            }
        }
        return temps;
    }

    std::pair<GMap<GString, DataType>, bool> try_derive_templates_no_err(
        GVector<DataType> targs,
        const GVector<std::pair<GString, DataType>> params,
        const Token& def,
        GVector<GString>* templates,
        const GVector<NodeExpr*> args,
        const Procedure& proc)
    {
        using __L_map = GMap<GString, DataType>;

        if (params.empty()) {
            return std::make_pair(__L_map{}, false);
        }
        if (params.size() != args.size()) {
            GeneratorError(def, "procedure `" + proc.name +
                                 "` excepts " + GString(std::to_string(params.size()).c_str()) +
                                 " args, but got " + GString(std::to_string(args.size()).c_str()) + ".");
        }

        assert(templates != NULL);
        assert(!templates->empty());

        __L_map temps;
        bool ok = derive_templates_core(targs, params, templates, args, proc, temps);
        if (!ok) {
            return std::make_pair(__L_map{}, false);
        }
        return std::make_pair(temps, true);
    }

    void resolve_overrides(Procedure* proc, std::optional<NodeExpr*> args, const Token& def) {
        assert(proc != NULL);
        Procedure* cp = NULL;
        if (!proc->overrides.empty()) {
            if (!args.has_value()) {
                if (!proc->params.empty() && proc->overrides.size() == 0)
                    GeneratorError(def, "procedure `" + proc->name + "` excepts " + GString(std::to_string(proc->params.size()).c_str()) + " arguments, but got 0.");
                if (proc->params.empty()) return;
                for (int i = 0; i < static_cast<int>(proc->overrides.size()); ++i) {
                    cp = proc->overrides[i];
                    if (cp->params.size() == 0ULL) {
                        *proc = *cp;
                        return;
                    }
                }
            } else {
                for (int i = 0; i < static_cast<int>(proc->overrides.size()); ++i) {
                    cp = proc->overrides[i];
                    if (__try_typecheck_call(__getargs(args.value()), *cp)) {
                        *proc = *cp;
                        return;
                    }
                }
            }
        }
        if (!proc->overrides.empty()) {
            if (!__try_typecheck_call(__getargs(args.value()), *proc)) {
                GVector<NodeExpr*> args_v = __getargs(args.value());
                GVector<DataType>  arg_types;
                for (auto* e : args_v) arg_types.push_back(canonical_type(type_of_expr(e)));
                GString args_s = format_type_list(arg_types);
                DiagnosticMessage(def, "error", "no match candidate for call procedure " + proc->name + "(" + args_s + ").", 0);
                GString args_s1;
                for (int i = 0; i < static_cast<int>(proc->params.size()); ++i) {
                    GString tmp(proc->params[i].second.to_string());
                    args_s1 += tmp.substr(1, tmp.size() - 2);
                    if (i != static_cast<int>(proc->params.size()) - 1) {
                        args_s1 += ", ";
                    }
                }
                DiagnosticMessage(proc->def, "note", "candidate " + proc->name + "(" + args_s1 + ").", 0);
                for (int i = 0; i < static_cast<int>(proc->overrides.size()); ++i) {
                    Procedure* cp2 = proc->overrides[i];
                    GString args_s2;
                    for (int j = 0; j < static_cast<int>(cp2->params.size()); ++j) {
                        GString tmp(cp2->params[j].second.to_string());
                        args_s2 += tmp.substr(1, tmp.size() - 2);
                        if (j != static_cast<int>(cp2->params.size()) - 1) {
                            args_s2 += ", ";
                        }
                    }
                    DiagnosticMessage(cp2->def, "note", "candidate " + proc->name + "(" + args_s2 + ").", 0);
                }
                exit(1);
            }
        }
    }

    GVector<Procedure> collect_candidates_earg(Procedure* proc) {
        GVector<Procedure> res;
        if (proc->params.empty()) res.push_back(*proc);
        for (int i = 0; i < static_cast<int>(proc->overrides.size()); ++i) {
            if (proc->overrides[i]->params.empty()) res.push_back(*proc->overrides[i]);
        }
        return res;
    }

    GVector<Procedure> collect_candidates(Procedure* proc, size_t args_size) {
        GVector<Procedure> res;
        if (proc->params.size() == args_size) res.push_back(*proc);
        for (int i = 0; i < static_cast<int>(proc->overrides.size()); ++i) {
            if (proc->overrides[i]->params.size() == args_size) res.push_back(*proc->overrides[i]);
        }
        return res;
    }

    void resolve_overrides_tp(Procedure* proc,
                              std::optional<NodeExpr*> args,
                              const Token& def,
                              GVector<DataType> targs)
    {
        assert(proc != NULL);
        Procedure  copy_of_proc = *proc;
        Procedure* ptr_to_proc  = proc;
        GVector<Procedure> candidates;
        if (args.has_value())
            candidates = collect_candidates(ptr_to_proc, __getargs(args.value()).size());
        else {
            candidates = collect_candidates_earg(ptr_to_proc);
            for (int i = 0; i < static_cast<int>(candidates.size()); ++i) {
                if (candidates[i].params.empty()) {
                    *ptr_to_proc = candidates[i];
                    return;
                }
            }
        }
        for (int i = 0; i < static_cast<int>(candidates.size()); ++i) {
            Procedure cur_c = candidates[i];
            GMap<GString, DataType> temps;
            if (cur_c.templates != NULL && targs.empty()) {
                assert(args.has_value());
                auto res = try_derive_templates_no_err(targs, cur_c.params, def, cur_c.templates, __getargs(args.value()), cur_c);
                if (!res.second) {
                    continue;
                } else temps = std::move(res.first);
            } else if (cur_c.templates != NULL && cur_c.templates->size() != targs.size()) {
                continue;
            } else if (cur_c.templates != NULL && targs.size() == cur_c.templates->size()) {
                size_t counter{ 0 };
                for (auto&& el : *cur_c.templates) {
                    temps[el] = targs[counter];
                    counter++;
                }
            }
            if (cur_c.templates != NULL) {
                substitute_template_params(temps, cur_c.params);
            }
            assert(args.has_value());
            if (__try_typecheck_call(__getargs(args.value()), cur_c)) {
                *ptr_to_proc = candidates[i];
                return;
            }
        }
        GString args_s = "";
        if(args.has_value()) {
            GVector<NodeExpr*> args_v = __getargs(args.value());
            GVector<DataType>  arg_types;
            for (auto* e : args_v) arg_types.push_back(canonical_type(type_of_expr(e)));
            args_s = format_type_list(arg_types);
        }
        DiagnosticMessage(def, "error", "no match candidate for call procedure " + proc->name + "(" + args_s + ").", 0);
        GString args_s1;
        for (int i = 0; i < static_cast<int>(proc->params.size()); ++i) {
            args_s1 += proc->params[i].second.to_string();
            if (i != static_cast<int>(proc->params.size()) - 1) {
                args_s1 += ", ";
            }
        }
        DiagnosticMessage(proc->def, "note", "candidate " + proc->name + "(" + args_s1 + ").", 0);
        for (int i = 0; i < static_cast<int>(proc->overrides.size()); ++i) {
            Procedure* cp = proc->overrides[i];
            GString args_s2;
            for (int j = 0; j < static_cast<int>(cp->params.size()); ++j) {
                args_s2 += cp->params[j].second.to_string();
                if (j != static_cast<int>(cp->params.size()) - 1) {
                    args_s2 += ", ";
                }
            }
            DiagnosticMessage(cp->def, "note", "candidate " + proc->name + "(" + args_s2 + ").", 0);
        }
        exit(1);
    }

    void gen_args(const GVector<NodeExpr*>& args,
              const GVector<std::pair<GString, DataType>>& params,
              const Token& where)
    {
        for (int i = static_cast<int>(args.size()) - 1; i >= 0; --i) {
            if (i < static_cast<int>(params.size())) {
                DataType from = type_of_expr(args[i]);
                DataType to   = params[i].second;

                DataType from_c = canonical_type(from);
                DataType to_c   = canonical_type(to);

                if (from_c.root().link) {
                    gen_expr(args[i], false);
                } else {
                    gen_expr(args[i], to_c.root().link);
                }

                if (is_interface_type(to_c) && !is_interface_type(from_c)) {
                    gen_box_interface(to_c, from_c, where);
                }
            } else {
                gen_expr(args[i], false);
            }
        }
    }

    void substitute_template_params(GMap<GString, DataType>& temps,
                                    GVector<std::pair<GString, DataType>>& params) {
        for (int i = 0; i < static_cast<int>(params.size()); ++i) {
            substitute_template_wct(params[i].second, temps);
        }
    }

    void gen_stmt(const NodeStmt* stmt)
    {
        struct StmtVisitor {
            Generator& gen;

            void operator()(const NodeStmtExit* stmt_exit) const
            {
                DataType etype = gen.type_of_expr(stmt_exit->expr);
                if (etype != BaseDataTypeInt) {
                    gen.GeneratorError(stmt_exit->def, "`exit` except type `int`\nNOTE: but got type " + etype.to_string());
                }
                gen.gen_expr(stmt_exit->expr);
                gen.m_builder.call(gen.sym("ExitProcess"));
            }

            void operator()(const NodeStmtProc* stmt_proc)
            {
                std::optional<Procedure> proc = gen.proc_lookup(stmt_proc->name);
                bool override = false;
                Procedure* movs = NULL;

                if (proc.has_value() && !gen.in_namespace()) {
                    if (proc.value().prototype) {
                        if (!gen.proc_same_signature(stmt_proc, proc.value())) {
                            gen.GeneratorError(stmt_proc->def,
                                "prototype of procedure and definition have different signature.");
                        }
                    } else {
                        if (gen.proc_same_signature(stmt_proc, proc.value())) {
                            Token pdef = proc.value().def;
                            gen.DiagnosticMessage(stmt_proc->def, "error",
                                                  "procedure `" + stmt_proc->name + "` redefenition.", 0);
                            gen.DiagnosticMessage(pdef, "note", "first definition here.", 0);
                            exit(1);
                        } else {
                            Procedure* ovs = gen.m_allocator->emplace<Procedure>();
                            ovs->name         = stmt_proc->name;
                            ovs->params       = stmt_proc->params;
                            ovs->rettype      = stmt_proc->rettype;
                            ovs->stack_allign = stmt_proc->params.size() +
                                                gen.collect_alligns(stmt_proc->scope);
                            ovs->attrs        = stmt_proc->attrs;
                            ovs->def          = stmt_proc->def;
                            ovs->prototype    = stmt_proc->prototype;
                            ovs->override     = true;
                            ovs->uniq_sign    = std::nullopt;
                            ovs->scope        = stmt_proc->scope;
                            ovs->from         = stmt_proc;
                            ovs->templates    = stmt_proc->templates;
                            ovs->overload_nth = gen.m_procs[stmt_proc->name].overrides.size() + 1;
                            movs = ovs;
                            gen.m_procs[stmt_proc->name].overrides.push_back(ovs);
                            override = true;
                        }
                    }
                }
                else if (proc.has_value() && proc.value().prototype && !gen.in_namespace()) {
                    if (proc.value().params.size() != stmt_proc->params.size()) {
                        gen.GeneratorError(stmt_proc->def,
                            "prototype of procedure and definition have different params sizes.\nNOTE: except `" +
                            GString(std::to_string(proc.value().params.size()).c_str()) + "` but got `" +
                            GString(std::to_string(stmt_proc->params.size()).c_str()) + "`.");
                    }
                    if (proc.value().rettype != stmt_proc->rettype) {
                        gen.GeneratorError(stmt_proc->def,
                            "prototype of procedure and defenition have other return types.\nNOTE: prototype return type - " +
                            proc.value().rettype.to_string() +
                            "\nNOTE: defenition return type - " +
                            stmt_proc->rettype.to_string());
                    }
                }

                size_t fsz = 0;
                if (!stmt_proc->prototype && stmt_proc->rettype != BaseDataTypeConst) {
                    fsz = gen.collect_alligns(stmt_proc->scope);
                }

                if (!override) {
                    if (gen.in_namespace()) {
                        const auto& search = gen.m_cur_namespace->procs.find(stmt_proc->name);
                        if (search != gen.m_cur_namespace->procs.end() && !(search->second.prototype)) {
                            Procedure& npc = search->second;

                            auto same_sig = [&](Procedure& p) -> bool {
                                if (p.params.size() != stmt_proc->params.size()) return false;
                                if (p.rettype != stmt_proc->rettype) return false;
                                if (p.params != stmt_proc->params)   return false;
                                if ((p.templates == NULL) != (stmt_proc->templates == NULL)) return false;
                                if (p.templates && stmt_proc->templates) {
                                    if (*p.templates != *stmt_proc->templates) return false;
                                }
                                return true;
                            };

                            if (same_sig(npc)) {
                                Token pdef = npc.def;
                                gen.DiagnosticMessage(stmt_proc->def, "error",
                                                      "procedure `" + stmt_proc->name + "` redefenition.", 0);
                                gen.DiagnosticMessage(pdef, "note", "first defenition here.", 0);
                                exit(1);
                            }

                            for (int i = 0; i < static_cast<int>(npc.overrides.size()); ++i) {
                                Procedure* cp = npc.overrides[i];
                                if (same_sig(*cp)) {
                                    Token pdef = cp->def;
                                    gen.DiagnosticMessage(stmt_proc->def, "error",
                                                          "procedure `" + stmt_proc->name + "` redefenition.", 0);
                                    gen.DiagnosticMessage(pdef, "note", "first defenition here.", 0);
                                    exit(1);
                                }
                            }

                            Procedure* ovs = gen.m_allocator->emplace<Procedure>();
                            ovs->name         = stmt_proc->name;
                            ovs->params       = stmt_proc->params;
                            ovs->rettype      = stmt_proc->rettype;
                            ovs->stack_allign = stmt_proc->params.size() +
                                                gen.collect_alligns(stmt_proc->scope);
                            ovs->attrs        = stmt_proc->attrs;
                            ovs->def          = stmt_proc->def;
                            ovs->prototype    = stmt_proc->prototype;
                            ovs->override     = true;
                            ovs->uniq_sign    = std::nullopt;
                            ovs->scope        = stmt_proc->scope;
                            ovs->from         = stmt_proc;
                            ovs->templates    = stmt_proc->templates;
                            ovs->overload_nth = npc.overrides.size() + 1;
                            movs              = ovs;
                            npc.overrides.push_back(ovs);
                            override = true;
                        } else {
                            gen.m_cur_namespace->procs[stmt_proc->name] = {
                                .name         = stmt_proc->name,
                                .params       = stmt_proc->params,
                                .rettype      = stmt_proc->rettype,
                                .stack_allign = stmt_proc->params.size() + fsz,
                                .attrs        = stmt_proc->attrs,
                                .def          = stmt_proc->def,
                                .prototype    = stmt_proc->prototype,
                                .overrides    = {},
                                .override     = false,
                                .uniq_sign    = std::nullopt,
                                .templates    = stmt_proc->templates,
                                .scope        = stmt_proc->scope,
                                .from         = stmt_proc,
                                .instanceated = {},
                                .mbn          = "",
                                .overload_nth = 0
                            };
                        }
                        if (stmt_proc->prototype) return;
                    } else {
                        gen.m_procs[stmt_proc->name] = {
                            .name         = stmt_proc->name,
                            .params       = stmt_proc->params,
                            .rettype      = stmt_proc->rettype,
                            .stack_allign = stmt_proc->params.size() + fsz,
                            .attrs        = stmt_proc->attrs,
                            .def          = stmt_proc->def,
                            .prototype    = stmt_proc->prototype,
                            .overrides    = {},
                            .override     = false,
                            .uniq_sign    = std::nullopt,
                            .templates    = stmt_proc->templates,
                            .scope        = stmt_proc->scope,
                            .from         = stmt_proc,
                            .instanceated = {},
                            .mbn          = "",
                            .overload_nth = 0
                        };
                    }
                }

                if (stmt_proc->rettype == BaseDataTypeConst) return;
                if (stmt_proc->templates != NULL)            return;
                if (stmt_proc->prototype) {
                    GVector<ProcAttr> attrs = stmt_proc->attrs;
                    bool is_cimport = std::find(attrs.begin(), attrs.end(), ProcAttr::cimport) != attrs.end();
                    if (is_cimport) {
                        gen.m_cexterns.push_back(stmt_proc->name);
                    }
                    return;
                }
                if (override && movs->templates != NULL)     return;

                GVector<ProcAttr> attrs = stmt_proc->attrs;
                bool noprolog = std::find(attrs.begin(), attrs.end(), ProcAttr::noprolog) != attrs.end();

                GString label;
                if (stmt_proc->name != "main") {
                    if (gen.in_namespace()) {
                        GString ns_asm = gen.mangle_ns_name(gen.m_cur_namespace->name);
                        label += ns_asm;
                        label += "@";
                    }
                    label += stmt_proc->name;
                    if (override) label += movs->get_sign();
                } else {
                    label = "__main";
                }
                gen.m_builder.label(label);
                gen.m_used_labels.insert(label);


                if (!noprolog) {
                    gen.m_builder.push(gen.reg(Reg::EBP));
                    gen.m_builder.mov(gen.reg(Reg::EBP), gen.reg(Reg::ESP));
                }

                gen.m_builder.call(gen.sym("__bpm_proc_enter"));

                if (!override) {
                    if (gen.in_namespace())
                        gen.m_cur_proc = gen.m_cur_namespace->procs[stmt_proc->name];
                    else
                        gen.m_cur_proc = gen.m_procs[stmt_proc->name];
                } else {
                    gen.m_cur_proc = *movs;
                }

                if (!override) {
                    if (gen.in_namespace())
                        gen.gen_traceback_push_nm(gen.m_cur_proc.value(), gen.m_cur_namespace->name);
                    else
                        gen.gen_traceback_push(gen.m_cur_proc.value());
                } else {
                    gen.gen_traceback_push(*movs);
                }

                if (stmt_proc->name == "main") {
                    gen.m_builder.call(gen.sym("_BPM_init_"));
                }

                gen.gen_scope_sp(stmt_proc->scope, stmt_proc, gen.m_cur_proc.value());
                gen.m_cur_proc = std::nullopt;

                if (stmt_proc->rettype.root() == BaseDataTypeVoid) {
                    gen.m_builder.call(gen.sym("__bpm_proc_leave"));
                    gen.m_builder.call(gen.sym("traceback_pop"));
                } else {
                    gen.push_reg(Reg::EAX);
                    gen.m_builder.call(gen.sym("__bpm_proc_leave"));
                    gen.m_builder.call(gen.sym("traceback_pop"));
                    gen.pop_reg(Reg::EAX);
                }

                if (stmt_proc->name == "main") {
                    gen.m_builder.mov(gen.reg(Reg::EAX), gen.imm(0));
                }

                if (!noprolog) {
                    gen.m_builder.pop(gen.reg(Reg::EBP));
                }

                gen.m_builder.ret();
                gen.m_var_index = 0U;
            }

            void operator()(const NodeStmtReturn* stmt_return) const
            {
                std::optional<Procedure> cproc = gen.m_cur_proc;
                if (!cproc.has_value()) {
                    gen.GeneratorError(stmt_return->def, "return without procedure");
                }
                DataType rettype = cproc.value().rettype;
                gen.substitute_template(rettype);
                if (rettype.root() == BaseDataTypeVoid && stmt_return->expr.has_value()) {
                    gen.GeneratorError(stmt_return->def, "return from void procedure with value");
                } else if (!stmt_return->expr.has_value() && rettype.root() != BaseDataTypeVoid) {
                    gen.GeneratorError(stmt_return->def, "procedure `" + cproc.value().name + "` at return except type " + rettype.to_string() + "\nNOTE: but got empty return");
                }
                if (stmt_return->expr.has_value() &&
                    gen.type_of_expr(stmt_return->expr.value()) != rettype)
                {
                    gen.GeneratorError(stmt_return->def, "procedure `" + cproc.value().name +
                        "` at return except type " + rettype.to_string() +
                        "\nNOTE: but got type " +
                        gen.type_of_expr(stmt_return->expr.value()).to_string());
                }
                if (stmt_return->expr.has_value()) {
                    if (std::holds_alternative<NodeTerm*>(stmt_return->expr.value()->var)) {
                        NodeTerm* t = std::get<NodeTerm*>(stmt_return->expr.value()->var);
                        if (std::holds_alternative<NodeTermAmpersand*>(t->var)) {
                            NodeTermAmpersand* as_amp = std::get<NodeTermAmpersand*>(t->var);
                            if (std::holds_alternative<NodeTerm*>(as_amp->expr->var)) {
                                NodeTerm* tt = std::get<NodeTerm*>(as_amp->expr->var);
                                if (std::holds_alternative<NodeTermIdent*>(tt->var)) {
                                    NodeTermIdent* id = std::get<NodeTermIdent*>(tt->var);
                                    GString name = id->ident.value.value();
                                    if (gen.var_lookup_cs(name).has_value()) {
                                        gen.GeneratorWarning(stmt_return->def,
                                            "Taking address of local variable `" + name + "`. "
                                            "Ensure this pointer does not outlive the scope.");
                                    }
                                }
                            }
                        }
                    }

                    gen.gen_expr(stmt_return->expr.value());
                    gen.pop_reg(Reg::EAX);
                }
                if (gen.in_namespace())
                    cproc.value().gen_ret(gen, gen.m_cur_namespace->name);
                else
                    cproc.value().gen_ret(gen, std::nullopt);
            }

            void operator()(const NodeStmtLet* stmt_let) const
            {
                if(stmt_let->expr.has_value())
                    gen.create_var(stmt_let->ident.value.value(), stmt_let->expr.value(),
                                    stmt_let->ident, stmt_let->type);
                else
                    gen.create_var_va(stmt_let->ident.value.value(), stmt_let->type.value(), stmt_let->ident);
            }

            void operator()(const NodeStmtCompileTimeIf* stmt_ctif) const {
                bool condition = gen.eval(stmt_ctif->condition, stmt_ctif->def);
                if (condition) {
                    for (const auto stmt : stmt_ctif->_if->stmts) {
                        gen.gen_stmt(stmt);
                    }
                } else {
                    if (stmt_ctif->_else.has_value()) {
                        for (const auto stmt : stmt_ctif->_else.value()->stmts) {
                            gen.gen_stmt(stmt);
                        }
                    }
                }
            }

            void operator()(const NodeStmtAssign* stmt_assign) const
            {
                NodeExpr* lvalue = stmt_assign->lvalue;
                DataType  ltype  = gen.type_of_expr(stmt_assign->lvalue);
                DataType  vtype  = gen.type_of_expr(stmt_assign->expr);

                if (ltype.root().link && vtype.root().link)
                    gen.GeneratorError(stmt_assign->def, "reference variable reassigning.");

                if (!ltype.root().is_object) {
                    auto nonobj_compatible = [&](const DataType& L, const DataType& R) -> bool {
                        if (!L.is_object() && !R.is_object())
                            return L.root().arg_eq(R.root());
                        return L == R;
                    };

                    if (!nonobj_compatible(ltype, vtype)) {
                        if (ltype.root().link && !vtype.root().link) {
                            NodeTermUnref unr;
                            unr.def  = stmt_assign->def;
                            unr.expr = lvalue;
                            NodeTerm at;
                            at.var = &unr;
                            NodeExpr ae;
                            ae.var = &at;

                            NodeStmtAssign newas;
                            newas.def    = stmt_assign->def;
                            newas.lvalue = &ae;
                            newas.expr   = stmt_assign->expr;
                            NodeStmt stmt;
                            stmt.var = &newas;
                            gen.gen_stmt(&stmt);
                            return;
                        } else {
                            gen.GeneratorError(stmt_assign->def,
                                "at = except type " + ltype.to_string() +
                                "\nNOTE: but got " + vtype.to_string());
                        }
                    }

                    if (std::holds_alternative<NodeTerm*>(lvalue->var)) {
                        NodeTerm* lvterm = std::get<NodeTerm*>(lvalue->var);
                        if (std::holds_alternative<NodeTermIdent*>(lvterm->var)) {
                            NodeTermIdent* lvident = std::get<NodeTermIdent*>(lvterm->var);
                            GString name = lvident->ident.value.value();
                            std::optional<Var> var = gen.var_lookup(name);
                            if (var.has_value()) {
                                gen.gen_expr(stmt_assign->expr);
                                gen.pop_reg(Reg::EDX);
                                gen.m_builder.mov(
                                    gen.mem(gen.local_mem(var.value().stack_loc)),
                                    gen.reg(Reg::EDX)
                                );
                                return;
                            }
                            std::optional<GVar> ivar = gen.gvar_lookup(name);
                            if (ivar.has_value()) {
                                gen.gen_expr(stmt_assign->expr);
                                gen.pop_reg(Reg::EDX);
                                gen.m_builder.mov(
                                    gen.mem(gen.global_mem(ivar.value().name)),
                                    gen.reg(Reg::EDX)
                                );
                                return;
                            }
                            gen.GeneratorError(stmt_assign->def,
                                               "unkown variable `" + name + "` at assignment");
                            return;
                        }
                    }
                    gen.gen_expr(stmt_assign->lvalue, true);
                    gen.gen_expr(stmt_assign->expr);
                    gen.pop_reg(Reg::ECX);
                    gen.pop_reg(Reg::EDX);
                    gen.m_builder.mov(
                        gen.mem(MemRef::baseDisp(Reg::EDX, 0)),
                        gen.reg(Reg::ECX)
                    );
                } else {

                    if (gen.is_interface_type(ltype)) {
                        gen.gen_expr(stmt_assign->expr);

                        if (!gen.is_interface_type(vtype)) {
                            gen.gen_box_interface(ltype, vtype, stmt_assign->def);
                        }

                        gen.gen_expr(stmt_assign->lvalue, true);

                        gen.pop_reg(Reg::EDX); 
                        gen.pop_reg(Reg::ECX); 

                        gen.m_builder.mov(
                            gen.mem(MemRef::baseDisp(Reg::EDX, 0)),
                            gen.reg(Reg::ECX)
                        );
                        return;
                    }

                    GString objnm = ltype.getobjectname();
                    std::optional<Namespace*> _nms = gen.namespace_lookup(objnm);
                    // TODON
                    bool can_assign = true;
                    if(!_nms.has_value()) {
                        std::optional<Struct> __stc = gen.struct_lookup(objnm);
                        if(!__stc.has_value()) {
                            gen.GeneratorError(stmt_assign->def, "unkown type `" + ltype.to_string() + "`");
                        }
                        can_assign = false;
                    }

                    if(_nms.has_value()) {
                        Namespace* cnm = _nms.value();
                        auto search = cnm->procs.find("m_assign");
                        if(search == cnm->procs.end()) can_assign = false;
                        Procedure& assign_proc = search->second;
                        if(assign_proc.params.size() < 2) {
                            can_assign = false;
                        }
                    }

                    if(!can_assign) {
                        gen.GeneratorError(stmt_assign->def, "can't assign type `" + vtype.to_string() + "` to `" + ltype.to_string() + "` missing method " + objnm + "::m_assign(" + ltype.to_string() + ", " + ltype.to_string() + ").");
                    }

                    NodeStmtMtCall call;
                    call.def  = stmt_assign->def;
                    call.mt   = stmt_assign->lvalue;
                    call.name = "m_assign";

                    NodeBinExprArgs args;
                    args.args.push_back(stmt_assign->lvalue);
                    args.args.push_back(stmt_assign->expr);

                    NodeBinExpr ab;
                    ab.var = &args;

                    NodeExpr ae;
                    ae.var = &ab;

                    call.args = &ae;

                    NodeStmt stmt;
                    stmt.var = &call;
                    gen.gen_stmt(&stmt);
                }
            }

            void operator()(const NodeStmtIncBy* stmt_assign) const
            {
                DataType oneT = gen.type_of_expr(stmt_assign->lvalue);
                DataType twoT = gen.type_of_expr(stmt_assign->expr);
                if (!oneT.root().is_object) {
                    if (oneT != twoT &&
                        !(oneT.root() == BaseDataTypePtr && twoT.root() == BaseDataTypeInt)) {
                        gen.GeneratorError(stmt_assign->def,
                            "at += except type " + oneT.to_string() +
                            "\nNOTE: but got " + twoT.to_string());
                    }
                    if (auto ident = ptools::get::ident(stmt_assign->lvalue)) {
                        Var vr = gen.var_lookup_err(ident.value()->ident.value.value(),
                                                    stmt_assign->def);
                        gen.gen_expr(stmt_assign->expr);
                        gen.pop_reg(Reg::EDX);
                        gen.m_builder.add(
                            gen.mem(vr.mem()),
                            gen.reg(Reg::EDX)
                        );
                        return;
                    }
                    gen.gen_expr(stmt_assign->lvalue, true);
                    gen.gen_expr(stmt_assign->expr);
                    gen.pop_reg(Reg::ECX);
                    gen.pop_reg(Reg::EDX);
                    gen.m_builder.add(
                        gen.mem(MemRef::baseDisp(Reg::EDX, 0)),
                        gen.reg(Reg::ECX)
                    );
                } else {
                    NodeStmtMtCall call;
                    call.def = stmt_assign->def;
                    call.mt  = stmt_assign->lvalue;
                    call.name = "m_inc";
                    NodeBinExprArgs args;
                    args.args.push_back(stmt_assign->lvalue);
                    args.args.push_back(stmt_assign->expr);
                    NodeBinExpr ab;
                    ab.var = &args;
                    NodeExpr ae;
                    ae.var = &ab;
                    call.args = &ae;
                    NodeStmt stmt;
                    stmt.var = &call;
                    gen.gen_stmt(&stmt);
                }
            }

            void operator()(const NodeStmtDecBy* stmt_assign) const
            {
                DataType oneT = gen.type_of_expr(stmt_assign->lvalue);
                DataType twoT = gen.type_of_expr(stmt_assign->expr);
                if (!oneT.root().is_object) {
                    if (oneT != twoT &&
                        !(oneT.root() == BaseDataTypePtr && twoT.root() == BaseDataTypeInt)) {
                        gen.GeneratorError(stmt_assign->def,
                            "at -= except type " + oneT.to_string() +
                            "\nNOTE: but got " + twoT.to_string());
                    }
                    if (auto ident = ptools::get::ident(stmt_assign->lvalue)) {
                        Var vr = gen.var_lookup_err(ident.value()->ident.value.value(),
                                                    stmt_assign->def);
                        gen.gen_expr(stmt_assign->expr);
                        gen.pop_reg(Reg::EDX);
                        gen.m_builder.sub(
                            gen.mem(vr.mem()),
                            gen.reg(Reg::EDX)
                        );
                        return;
                    }
                    gen.gen_expr(stmt_assign->lvalue, true);
                    gen.gen_expr(stmt_assign->expr);
                    gen.pop_reg(Reg::ECX);
                    gen.pop_reg(Reg::EDX);
                    gen.m_builder.sub(
                        gen.mem(MemRef::baseDisp(Reg::EDX, 0)),
                        gen.reg(Reg::ECX)
                    );
                } else {
                    NodeStmtMtCall call;
                    call.def = stmt_assign->def;
                    call.mt  = stmt_assign->lvalue;
                    call.name = "m_dec";
                    NodeBinExprArgs args;
                    args.args.push_back(stmt_assign->lvalue);
                    args.args.push_back(stmt_assign->expr);
                    NodeBinExpr ab;
                    ab.var = &args;
                    NodeExpr ae;
                    ae.var = &ab;
                    call.args = &ae;
                    NodeStmt stmt;
                    stmt.var = &call;
                    gen.gen_stmt(&stmt);
                }
            }

            void operator()(const NodeStmtMulBy* stmt_assign) const
            {
                gen.gen_expr(stmt_assign->lvalue, true);
                gen.gen_expr(stmt_assign->expr);
                gen.pop_reg(Reg::ECX);
                gen.pop_reg(Reg::EDX);
                gen.m_builder.mov(
                    gen.reg(Reg::EAX),
                    gen.mem(MemRef::baseDisp(Reg::EDX, 0))
                );
                gen.m_builder.emit(IRInstr(IROp::IMul, gen.reg(Reg::EAX), gen.reg(Reg::ECX)));
                gen.m_builder.mov(
                    gen.mem(MemRef::baseDisp(Reg::EDX, 0)),
                    gen.reg(Reg::EAX)
                );
            }

            void operator()(const NodeStmtDivBy* stmt_assign) const
            {
                gen.gen_expr(stmt_assign->lvalue, true);
                gen.gen_expr(stmt_assign->expr);
                gen.pop_reg(Reg::EDI);
                gen.pop_reg(Reg::ESI);
                gen.m_builder.emit(IRInstr(IROp::Xor, gen.reg(Reg::EDX), gen.reg(Reg::EDX)));
                gen.m_builder.mov(
                    gen.reg(Reg::EAX),
                    gen.mem(MemRef::baseDisp(Reg::ESI, 0))
                );
                gen.m_builder.emit(IRInstr(IROp::Div, gen.reg(Reg::EDI)));
                gen.m_builder.mov(
                    gen.mem(MemRef::baseDisp(Reg::ESI, 0)),
                    gen.reg(Reg::EAX)
                );
            }

            void operator()(NodeStmtCall* stmt_call) const
            {
                const GString name = stmt_call->name;
                Procedure proc = gen.__proc_get(stmt_call->name, stmt_call->def);
                if (!proc.overrides.empty())
                    gen.resolve_overrides_tp(&proc, stmt_call->args, stmt_call->def, stmt_call->targs);

                GVector<NodeExpr*> raw_args;
                if (stmt_call->args.has_value()) raw_args = gen.__getargs(stmt_call->args.value());

                auto inst_res = gen.instantiate_if_needed(proc, stmt_call->targs, raw_args, stmt_call->def, name, "");
                GString tsign = inst_res.first;
                auto temps = inst_res.second;

                size_t stack_allign = 0;
                Procedure proc_check = proc;
                gen.substitute_template_params(temps, proc_check.params);

                if (!raw_args.empty() || proc.params.empty()) {
                    if (!raw_args.empty())
                        gen.__typecheck_call(raw_args, proc_check.params, stmt_call->def,
                                             proc_check, &stack_allign);
                } else {
                    gen.GeneratorError(stmt_call->def, "procedure `" + proc.name + "` expects " +
                                                          GString(std::to_string(proc.params.size()).c_str()) +
                                                          " args, but got 0");
                }

                gen.gen_args(raw_args, proc_check.params, stmt_call->def);

                GString label = name + tsign;
                if (proc.override) label += proc.get_sign();
                gen.m_builder.call(gen.sym(label));

                if (stack_allign != 0)
                    gen.m_builder.add(gen.reg(Reg::ESP),
                                      gen.imm(static_cast<int32_t>(stack_allign * 4)));
            }

            void operator()(const NodeScope* scope) const
            {
                gen.gen_scope(scope);
            }

            void operator()(const NodeStmtPushOnStack* stmt_push) const
            {
                gen.gen_expr(stmt_push->expr);
            }

            void operator()(const NodeStmtIf* stmt_if) const
            {
                gen.gen_expr(stmt_if->expr);
                gen.pop_reg(Reg::EAX);
                const GString label = gen.create_label();
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EAX), gen.reg(Reg::EAX)));
                gen.m_builder.jz(gen.label(label));
                gen.gen_scope(stmt_if->scope);
                if (stmt_if->pred.has_value()) {
                    const GString end_label = gen.create_label();
                    gen.m_builder.jmp(gen.label(end_label));
                    gen.m_builder.label(label);
                    gen.gen_if_pred(stmt_if->pred.value(), end_label);
                    gen.m_builder.label(end_label);
                } else {
                    gen.m_builder.label(label);
                }
            }

            void operator()(const NodeStmtWhile* stmt_while) const
            {
                auto preiflab = gen.create_label();
                auto blocklab = gen.create_label();
                auto breaklab = gen.create_label();
                auto endlab   = gen.create_label();
                gen.m_breaks.push_back(breaklab);

                gen.m_builder.label(preiflab);
                gen.gen_expr(stmt_while->expr);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EAX), gen.reg(Reg::EAX)));
                gen.m_builder.jz(gen.label(endlab));

                gen.m_builder.label(blocklab);
                gen.gen_scope(stmt_while->scope, 0, true);
                gen.m_builder.jmp(gen.label(preiflab));

                gen.m_builder.label(breaklab);
                if (gen.m_break_scopes[gen.m_break_scopes.size() - 1] != 0ULL) {
                    gen.m_builder.add(
                        gen.reg(Reg::ESP),
                        gen.imm(static_cast<int32_t>(
                            gen.m_break_scopes[gen.m_break_scopes.size() - 1] * 4))
                    );
                }
                gen.m_builder.label(endlab);
                gen.m_breaks.pop_back();
                gen.m_break_scopes.pop_back();
            }

            void operator()(const NodeStmtBreak* stmt_break) const
            {
                if (gen.m_breaks.size() == 0ULL) {
                    gen.GeneratorError(stmt_break->def, "break without loop");
                }
                gen.m_builder.jmp(gen.label(gen.m_breaks[gen.m_breaks.size() - 1ULL]));
            }

            void operator()(const NodeStmtStore* stmt_store) const
            {
                DataType ptype = gen.type_of_expr(stmt_store->ptr);
                if (ptype.root() != BaseDataTypePtr && ptype.root().ptrlvl == 0ULL) {
                    gen.GeneratorError(stmt_store->def,
                        "store types missmatch\nNOTE: except `ptr`, `T`\nNOTE: but got " +
                        ptype.to_string() + ", " + "`T`");
                }
                if (stmt_store->size == 8U) {
                    gen.gen_expr(stmt_store->ptr);
                    gen.gen_expr(stmt_store->expr);
                    gen.pop_reg(Reg::EDX);
                    gen.pop_reg(Reg::ECX);
                    Operand memop = gen.mem(MemRef::baseDisp(Reg::ECX, 0));
                    gen.m_builder.emit(IRInstr(IROp::Store8, memop, gen.reg(Reg::EDX)));
                } else if (stmt_store->size == 16U) {
                    gen.gen_expr(stmt_store->ptr);
                    gen.gen_expr(stmt_store->expr);
                    gen.pop_reg(Reg::EDX);
                    gen.pop_reg(Reg::ECX);
                    Operand memop = gen.mem(MemRef::baseDisp(Reg::ECX, 0));
                    gen.m_builder.emit(IRInstr(IROp::Store16, memop, gen.reg(Reg::EDX)));
                } else if (stmt_store->size == 32U) {
                    gen.gen_expr(stmt_store->ptr);
                    gen.gen_expr(stmt_store->expr);
                    gen.pop_reg(Reg::EDX);
                    gen.pop_reg(Reg::ECX);
                    gen.m_builder.mov(
                        gen.mem(MemRef::baseDisp(Reg::ECX, 0)),
                        gen.reg(Reg::EDX)
                    );
                } else {
                    assert(false);
                }
            }

            void operator()(const NodeStmtBuffer* stmt_buf) const
            {
                int size = gen.eval(stmt_buf->size, stmt_buf->def);
                if (size % 2 != 0) {
                    gen.GeneratorError(stmt_buf->def, "size of buffer must be a even number");
                }
                gen.m_var_index += (size / 4);
                gen.create_var_va_wid(stmt_buf->name, BaseDataTypeChar, stmt_buf->def);
            }

            void operator()(const NodeStmtAsm* stmt_asm) const
            {
                gen.m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp(stmt_asm->code)));
            }

            void operator()(const NodeStmtCextern* stmt_cextern) const
            {
                if (std::find(gen.m_cexterns.begin(), gen.m_cexterns.end(), stmt_cextern->name) !=
                    gen.m_cexterns.end()) {
                    return;
                }
                gen.m_cexterns.push_back(stmt_cextern->name);
            }

            void operator()(const NodeStmtStruct* stmt_struct) const
            {
                std::optional<Struct> alrh = gen.struct_lookup(stmt_struct->name);
                if (alrh.has_value()) {
                    gen.DiagnosticMessage(stmt_struct->def, "error",
                        "redefinition of structure `" + stmt_struct->name + "`", strlen("struct "));
                    gen.DiagnosticMessage(alrh.value().def, "note", "first defenition here", strlen("struct "));
                    exit(EXIT_FAILURE);
                }

                GVector<std::pair<GString, DataType>> final_fields;
                std::optional<DataType> parent_dt = stmt_struct->parent;

                if (parent_dt.has_value()) {
                    GString pname = parent_dt.value().getobjectname();
                    std::optional<Struct> p_struct_opt = gen.struct_lookup(pname);
                    
                    if (!p_struct_opt.has_value()) {
                        gen.GeneratorError(stmt_struct->def, "parent struct `" + pname + "` not found");
                    }
                    
                    Struct p_struct = p_struct_opt.value();
                    
                    final_fields = p_struct.__fields;
                    
                    if (p_struct.temp) {
                        GVector<DataType> targs = gen.get_template_args(parent_dt.value());
                        
                        if (targs.size() != p_struct.temps.size()) {
                             gen.GeneratorError(stmt_struct->def, "parent struct template args mismatch");
                        }

                        GMap<GString, DataType> temps = gen.compute_temps(p_struct.temps, targs);
                        
                        for(auto& f : final_fields) {
                            gen.substitute_template_wct(f.second, temps);
                        }
                    }
                }
                
                final_fields.insert(final_fields.end(), stmt_struct->fields.begin(), stmt_struct->fields.end());

                size_t current_type_id = (*gen.m_typeid_table_size)++;
                gen.m_typeid_table.push_back(std::make_pair(current_type_id, stmt_struct->name));
                
                gen.m_structs[stmt_struct->name] = {
                    .name       = stmt_struct->name,
                    .fields     = gen.compute_fields(final_fields),
                    .__allocator = stmt_struct->__allocator,
                    .__fields   = final_fields,
                    .m_typeid   = gen.m_structs_count++,
                    .temp       = stmt_struct->temp,
                    .temps      = stmt_struct->temps,
                    .def        = stmt_struct->def,
                    .parent_type = stmt_struct->parent
                };
            }

            void operator()(const NodeStmtInterface* stmt_inter) const
            {
                Interface iface;
                iface.name    = stmt_inter->name;
                iface.m_typeid = gen.m_structs_count++;
                iface.temps   = stmt_inter->temps;

                for (const auto& m : stmt_inter->methods) {
                    InterfaceMethodInfo info;
                    info.name    = m.name;
                    info.params  = m.params;
                    info.rettype = m.rettype;
                    iface.methods[m.name] = std::move(info);
                    iface.method_order.push_back(m.name);
                }

                gen.m_interfaces[stmt_inter->name] = std::move(iface);
            }

            void operator()(const NodeStmtOninit* stmt_oninit) const
            {
                gen.__oninits.push_back(stmt_oninit->scope);
            }

            void operator()(const NodeStmtStaticAssert* stmt_st) const
            {
                if (!static_cast<bool>(gen.eval(stmt_st->condition, stmt_st->def))) {
                    gen.DiagnosticMessage(stmt_st->def, "AssertionFailed", stmt_st->msg,
                                          strlen("static_assert("));
                    exit(EXIT_FAILURE);
                }
            }

            void operator()(const NodeStmtDelete* stmt_delete) const
            {
                DataType type = gen.type_of_expr(stmt_delete->expr);
                if (!type.is_object()) {
                    gen.GeneratorError(stmt_delete->def, "`delete` except object\nNOTE: but got " +
                                                       gen.type_of_expr(stmt_delete->expr).to_string());
                }
                GString objectName = type.getobjectname();
                std::optional<Struct> st = gen.struct_lookup(objectName);
                if (st.has_value()) {
                    if (st.value().__allocator.has_value()) {
                        gen.GeneratorWarning(stmt_delete->def,
                            "objects of type `" + objectName + "` uses custom allocator function.\nNOTE: delete of object of this type may free youre arena-pool.");
                    }
                }
                std::optional<Namespace*> nm = gen.namespace_lookup(objectName);
                if (nm.has_value()) {
                    const auto& search = nm.value()->procs.find("destroy");
                    if (search != nm.value()->procs.end()) {
                        NodeStmtMtCall call;
                        call.def = stmt_delete->def;
                        call.mt  = stmt_delete->expr;
                        call.name = "destroy";
                        GVector<NodeExpr*> args;
                        args.push_back(stmt_delete->expr);
                        NodeBinExprArgs aa;
                        aa.args = args;
                        NodeBinExpr ab;
                        ab.var = &aa;
                        NodeExpr ae;
                        ae.var = &ab;
                        call.args = &ae;
                        NodeStmt stmt;
                        stmt.var = &call;
                        gen.gen_stmt(&stmt);
                    }
                }
                gen.gen_expr(stmt_delete->expr);
                gen.m_builder.call(gen.sym("memfree"));
                gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(4));
            }

            void operator()(const NodeStmtRaise* stmt_raise) const
            {
                DataType tp = gen.type_of_expr(stmt_raise->expr);
                bool has_what = false;
                if (tp.is_object()) {
                    GString oname = tp.root().getobjectname();
                    std::optional<Namespace*> nm = gen.namespace_lookup(oname);
                    if (nm.has_value()) {
                        Namespace* nms = nm.value();
                        const auto& search = nms->procs.find("what");
                        if (search != nms->procs.end()) {
                            Procedure proc = search->second;
                            has_what = true;
                            GString lbl = oname + "@what";
                            gen.push_sym(lbl);
                        }
                    }
                }
                if (!has_what) {
                    gen.push_imm(0);
                }
                gen.gen_expr(stmt_raise->expr);
                gen.push_imm(static_cast<int32_t>(gen.typeid_of(tp)));
                gen.m_builder.call(gen.sym("__bpm_allocate_exception"));
                gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(12));
                gen.push_reg(Reg::EAX);
                gen.m_builder.call(gen.sym("__bpm_throw"));
                gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(4));
            }

            void operator()(const NodeStmtNamespace* stmt_space) const
            {
                GString fullName;
                if (!gen.m_ns_stack.empty()) {
                    fullName = gen.m_ns_stack.back() + "::" + stmt_space->name;
                } else {
                    fullName = stmt_space->name;
                }

                Namespace* nm;
                auto enm = gen.namespace_lookup(fullName);
                if (!enm.has_value()) {
                    nm = gen.m_allocator->emplace<Namespace>();
                    nm->procs = {};
                    nm->name  = fullName;
                    gen.m_namespaces[fullName] = nm;
                } else {
                    nm = enm.value();
                }

                Namespace* prev_ns = gen.m_cur_namespace;

                gen.m_cur_namespace = nm;
                gen.m_ns_stack.push_back(fullName);

                for (NodeStmt* stmt : stmt_space->scope->stmts) {
                    gen.gen_stmt(stmt);
                }

                gen.m_ns_stack.pop_back();
                gen.m_cur_namespace = prev_ns;
            }

            void operator()(NodeStmtImpl* stmt_impl) const
            {
                std::optional<Namespace*> enm = gen.namespace_lookup(stmt_impl->name);
                GString iname = stmt_impl->name;
                std::optional<Struct> h_s = gen.struct_lookup(iname);
                if (!h_s.has_value())
                    gen.GeneratorError(stmt_impl->def, "unkown structure name `" + iname + "`.");
                if (enm.has_value()) {
                    gen.GeneratorError(stmt_impl->def, "redefenition of implementation struct `" + iname + "`.");
                }
                Namespace* nm = gen.m_allocator->emplace<Namespace>();
                nm->procs = {};
                nm->name  = stmt_impl->name;
                gen.m_cur_namespace = nm;
                gen.m_namespaces[nm->name] = nm;
                for (NodeStmt* stmt : stmt_impl->scope->stmts) {
                    if (!stmt_impl->temps.empty() && std::holds_alternative<NodeStmtProc*>(stmt->var)) {
                        NodeStmtProc* ps = std::get<NodeStmtProc*>(stmt->var);
                        GString pname = ps->name;
                        if (std::find(stmt_impl->inst.begin(), stmt_impl->inst.end(), pname) ==
                            stmt_impl->inst.end()) {
                            if (ps->templates == NULL)
                                ps->templates = gen.m_allocator->emplace<GVector<GString>>();
                            ps->templates->insert(ps->templates->begin(),
                                                  stmt_impl->temps.begin(), stmt_impl->temps.end());
                        }
                    }
                    gen.gen_stmt(stmt);
                }
                gen.m_cur_namespace = NULL;
            }

            void operator()(NodeStmtNmCall* stmt_call) const
            {
                __str_ref pname = stmt_call->name;
                __str_ref nname = stmt_call->nm;
                std::optional<Namespace*> nsp = gen.namespace_lookup(nname);
                if (!nsp.has_value())
                    gen.GeneratorError(stmt_call->def, "unkown namespace `" + nname + "`");
                const auto& search = nsp.value()->procs.find(pname);
                if (search == nsp.value()->procs.end())
                    gen.GeneratorError(stmt_call->def, "namespace `" + nname + "` doesn't have procedure `" + pname + "`");
                Procedure proc = search->second;

                if (!proc.overrides.empty()) {
                    gen.resolve_overrides_tp(&proc, stmt_call->args, stmt_call->def, stmt_call->targs);
                }

                GVector<NodeExpr*> raw_args;
                if (stmt_call->args.has_value()) raw_args = gen.__getargs(stmt_call->args.value());

                auto inst_res = gen.instantiate_if_needed(proc, stmt_call->targs, raw_args, stmt_call->def, pname, nname);
                GString tsign = inst_res.first;
                auto temps = inst_res.second;

                size_t stack_allign = 0;
                Procedure proc_check = proc;
                gen.substitute_template_params(temps, proc_check.params);

                if (!raw_args.empty() || proc.params.empty()) {
                    if (!raw_args.empty())
                        gen.__typecheck_call(raw_args, proc_check.params, stmt_call->def, proc_check, &stack_allign);
                } else {
                    gen.GeneratorError(stmt_call->def,
                        "procedure `" + proc.name + "` expects " +
                        GString(std::to_string(proc.params.size()).c_str()) + " args, but got 0");
                }

                gen.gen_args(raw_args, proc_check.params, stmt_call->def);

                GString label = gen.mangle_ns_name(nname) + "@" + pname + tsign;
                if (proc.override) label += proc.get_sign();
                gen.m_builder.call(gen.sym(label));

                if (stack_allign != 0)
                    gen.m_builder.add(gen.reg(Reg::ESP),
                                      gen.imm(static_cast<int32_t>(stack_allign * 4)));
            }

            void operator()(const NodeStmtMtCall* stmt_call) const
            {
                DataType tpof = gen.type_of_expr(stmt_call->mt);

                if (gen.is_interface_type(tpof)) {
                    NodeExpr* orig_args_expr = nullptr;
                    if (stmt_call->args.has_value()) {
                        orig_args_expr = stmt_call->args.value();
                    }

                    NodeExpr* final_args_expr = nullptr;

                    if (orig_args_expr &&
                        std::holds_alternative<NodeBinExpr*>(orig_args_expr->var) &&
                        std::holds_alternative<NodeBinExprArgs*>(
                            std::get<NodeBinExpr*>(orig_args_expr->var)->var))
                    {
                        NodeBinExpr* be = std::get<NodeBinExpr*>(orig_args_expr->var);
                        NodeBinExprArgs* orig_args = std::get<NodeBinExprArgs*>(be->var);

                        auto* new_args = gen.m_allocator->emplace<NodeBinExprArgs>();
                        for (size_t i = 0; i < orig_args->args.size(); ++i) {
                            NodeExpr* e = orig_args->args[i];
                            if (i == 0 && e == stmt_call->mt) {
                                continue;
                            }
                            new_args->args.push_back(e);
                        }

                        if (!new_args->args.empty()) {
                            NodeBinExpr* ab = gen.m_allocator->emplace<NodeBinExpr>();
                            ab->def = stmt_call->def;
                            ab->var = new_args;
                            final_args_expr = gen.m_allocator->emplace<NodeExpr>();
                            final_args_expr->var = ab;
                        } else {
                            final_args_expr = nullptr;
                        }
                    } else {
                        final_args_expr = orig_args_expr;
                    }

                    NodeTermCall* call_term = gen.m_allocator->emplace<NodeTermCall>();
                    call_term->def   = stmt_call->def;
                    call_term->name  = stmt_call->name;
                    call_term->targs = stmt_call->targs;
                    if (final_args_expr) {
                        call_term->args = final_args_expr;
                    } else {
                        call_term->args = std::nullopt;
                    }

                    NodeTerm* term = gen.m_allocator->emplace<NodeTerm>();
                    term->var = call_term;

                    NodeExpr* rhs_expr = gen.m_allocator->emplace<NodeExpr>();
                    rhs_expr->var = term;

                    NodeBinExprDot* dot = gen.m_allocator->emplace<NodeBinExprDot>();
                    dot->lhs = stmt_call->mt;
                    dot->rhs = rhs_expr;

                    NodeBinExpr* bexpr = gen.m_allocator->emplace<NodeBinExpr>();
                    bexpr->def = stmt_call->def;
                    bexpr->var = dot;

                    NodeExpr* expr = gen.m_allocator->emplace<NodeExpr>();
                    expr->var = bexpr;

                    gen.gen_expr(expr);

                    DataType rettype = gen.type_of_expr(expr);
                    if (rettype != BaseDataTypeVoid) {
                        gen.pop_reg(Reg::EAX);
                    }
                    return;
                }

                if (!tpof.is_object() || tpof.root().link) {
                    gen.GeneratorError(stmt_call->def,
                        "can't call method from type " + tpof.to_string() + ".");
                }

                NodeStmtNmCall* nmcall = gen.m_allocator->emplace<NodeStmtNmCall>();
                nmcall->def   = stmt_call->def;
                nmcall->nm    = tpof.getobjectname();
                nmcall->name  = stmt_call->name;
                nmcall->args  = stmt_call->args;
                nmcall->targs = stmt_call->targs;

                NodeStmt stmt;
                stmt.var = nmcall;
                gen.gen_stmt(&stmt);
            }

            void operator()(const NodeStmtConst* stmt_const) const
            {
                if (gen.m_cur_namespace != NULL) {
                    gen.m_cur_namespace->consts[stmt_const->name] = gen.eval(stmt_const->expr, stmt_const->def);
                }
                else {
                    gen.last_scope_cns()[stmt_const->name] = {
                        .name  = stmt_const->name,
                        .value = gen.eval(stmt_const->expr, stmt_const->def)
                    };
                }
            }

            void operator()(const NodeStmtTypedef* stmt_tdef) const
            {
                gen.last_scope_tdef()[stmt_tdef->name] = stmt_tdef->type;
            }

            void operator()(const NodeStmtTry* stmt_try) const
            {
                const GString catch_lab  = gen.create_label();
                const GString end_catch  = gen.create_label();
                const GString end_lab    = gen.create_label();

                gen.m_builder.emit(
                    IRInstr(IROp::Inc, gen.mem(MemRef::sym("__exception_bufs_lvl")))
                );

                gen.m_builder.mov(
                    gen.reg(Reg::EAX),
                    gen.mem(MemRef::sym("__exception_bufs_lvl"))
                );
                gen.m_builder.mov(
                    gen.reg(Reg::EDX),
                    gen.mem(MemRef::sym("traceback", 4096))
                );

                gen.m_builder.mov(gen.reg(Reg::ECX), gen.reg(Reg::EAX));
                gen.m_builder.emit(
                    IRInstr(IROp::IMul, gen.reg(Reg::ECX), gen.reg(Reg::ECX), gen.imm(4))
                );
                gen.m_builder.add(gen.reg(Reg::ECX), gen.sym("__traceback_saved"));

                gen.m_builder.mov(
                    gen.mem(MemRef::baseDisp(Reg::ECX, 0)),
                    gen.reg(Reg::EDX)
                );

                gen.m_builder.mov(
                    gen.reg(Reg::EAX),
                    gen.mem(MemRef::sym("__exception_bufs_lvl"))
                );
                gen.m_builder.emit(
                    IRInstr(IROp::IMul, gen.reg(Reg::EAX), gen.reg(Reg::EAX), gen.imm(64))
                );
                gen.m_builder.add(gen.reg(Reg::EAX), gen.sym("__exception_bufs"));
                gen.push_reg(Reg::EAX);
                gen.m_builder.call(gen.sym("_setjmp"));
                gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(4));

                gen.m_builder.emit(
                    IRInstr(IROp::Cmp, gen.reg(Reg::EAX), gen.imm(1))
                );
                gen.m_builder.jnz(gen.label(end_catch));
                gen.m_builder.label(catch_lab);


                gen.m_builder.mov(
                    gen.reg(Reg::EAX),
                    gen.mem(MemRef::sym("__exception_bufs_lvl"))
                );
                gen.m_builder.add(gen.reg(Reg::EAX), gen.imm(1));

                gen.m_builder.mov(gen.reg(Reg::ECX), gen.reg(Reg::EAX));
                gen.m_builder.emit(
                    IRInstr(IROp::IMul, gen.reg(Reg::ECX), gen.reg(Reg::ECX), gen.imm(4))
                );
                gen.m_builder.add(gen.reg(Reg::ECX), gen.sym("__traceback_saved"));

                gen.m_builder.mov(
                    gen.reg(Reg::EDX),
                    gen.mem(MemRef::baseDisp(Reg::ECX, 0))
                );

                gen.m_builder.mov(
                    gen.mem(MemRef::sym("traceback", 4096)),
                    gen.reg(Reg::EDX)
                );

                size_t catch_scope_size = 1 + gen.collect_alligns(stmt_try->_catch);
                gen.begin_scope(static_cast<int>(catch_scope_size));
                gen.create_var_va(stmt_try->name, stmt_try->type, stmt_try->def);
                Var vr = gen.var_lookup_cs(stmt_try->name).value();

                gen.m_builder.call(gen.sym("__bpm_get_current_exception"));
                gen.m_builder.mov(
                    gen.mem(gen.local_mem(vr.stack_loc)),
                    gen.reg(Reg::EAX)
                );

                for (const NodeStmt* st : stmt_try->_catch->stmts) {
                    gen.gen_stmt(st);
                }
                //gen.last_scope().erase(gen.last_scope().find(stmt_try->name));
                gen.end_scope();

                gen.m_builder.jmp(gen.label(end_lab));

                gen.m_builder.label(end_catch);

                gen.push_imm(static_cast<int32_t>(gen.typeid_of(stmt_try->type)));
                gen.m_builder.call(gen.sym("__bpm_start_catch"));
                gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(4));

                gen.gen_scope(stmt_try->_try);

                gen.m_builder.call(gen.sym("__bpm_end_catch"));

                gen.m_builder.mov(
                    gen.reg(Reg::EAX),
                    gen.mem(MemRef::sym("__exception_bufs_lvl"))
                );
                gen.m_builder.sub(gen.reg(Reg::EAX), gen.imm(1));
                gen.m_builder.mov(
                    gen.mem(MemRef::sym("__exception_bufs_lvl")),
                    gen.reg(Reg::EAX)
                );

                gen.m_builder.label(end_lab);
            }
            void operator()(const NodeStmtFor* stmt_for) const
            {
                size_t init_size = 0;
                if (stmt_for->init) {
                    NodeScope tmp_scope;
                    tmp_scope.stmts.push_back(stmt_for->init);
                    init_size = gen.collect_alligns(&tmp_scope);
                }
                
                size_t body_size = gen.collect_alligns(stmt_for->scope);
                
                gen.begin_scope(static_cast<int>(init_size + body_size));
                
                if (stmt_for->init) {
                    gen.gen_stmt(stmt_for->init);
                }
                
                auto start_label = gen.create_label();
                auto end_label   = gen.create_label();
                auto step_label  = gen.create_label();
                
                gen.m_breaks.push_back(end_label);
                gen.m_break_scopes.push_back(init_size + body_size);

                gen.m_builder.label(start_label);
                
                if (stmt_for->cond) {
                    gen.gen_expr(stmt_for->cond);
                    gen.pop_reg(Reg::EAX);
                    gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EAX), gen.reg(Reg::EAX)));
                    gen.m_builder.jz(gen.label(end_label));
                }
                
                for(const auto& st : stmt_for->scope->stmts) {
                    gen.gen_stmt(st);
                }
                
                gen.m_builder.label(step_label);
                if (stmt_for->step) {
                    gen.gen_stmt(stmt_for->step);
                }
                
                gen.m_builder.jmp(gen.label(start_label));
                
                gen.m_builder.label(end_label);
                
                gen.m_breaks.pop_back();
                gen.m_break_scopes.pop_back();
                
                gen.end_scope();
            }
            void operator()(const NodeStmtForeach* stmt_foreach) const
            {
                size_t body_size = gen.collect_alligns(stmt_foreach->scope);
                gen.begin_scope(static_cast<int>(4 + body_size));
                
                GString cont_name = "__foreach_cont_" + GString(std::to_string(gen.CTX_IOTA++).c_str());
                gen.create_var(cont_name, stmt_foreach->expr, stmt_foreach->var_name, std::nullopt);
                
                NodeTermIdent cont_ident; 
                cont_ident.ident = {TokenType_t::ident, 0, 0, cont_name, "", std::nullopt};
                NodeTerm cont_term; cont_term.var = &cont_ident;
                NodeExpr cont_expr; cont_expr.var = &cont_term;
                
                auto* call_begin_node = gen.make_method_call(&cont_expr, "begin", {}, stmt_foreach->var_name);
                GString it_name = "__foreach_it_" + GString(std::to_string(gen.CTX_IOTA++).c_str());
                gen.create_var(it_name, call_begin_node, stmt_foreach->var_name, std::nullopt);
                
                auto* call_end_node = gen.make_method_call(&cont_expr, "end", {}, stmt_foreach->var_name);
                GString end_name = "__foreach_end_" + GString(std::to_string(gen.CTX_IOTA++).c_str());
                gen.create_var(end_name, call_end_node, stmt_foreach->var_name, std::nullopt);
                
                auto start_label = gen.create_label();
                auto end_label   = gen.create_label();
                gen.m_breaks.push_back(end_label);
                gen.m_break_scopes.push_back(4 + body_size);
                
                gen.m_builder.label(start_label);
                
                auto* it_expr  = gen.make_ident_expr(it_name);
                auto* end_expr = gen.make_ident_expr(end_name);
                auto* neq_expr = gen.make_neq_expr(it_expr, end_expr, stmt_foreach->var_name);
                
                gen.gen_expr(neq_expr);
                gen.pop_reg(Reg::EAX);
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EAX), gen.reg(Reg::EAX)));
                gen.m_builder.jz(gen.label(end_label));
                
                auto* it_expr_deref = gen.make_ident_expr(it_name);
                auto* deref_expr = gen.make_deref_expr(it_expr_deref, stmt_foreach->var_name);
                
                GString user_var_name = stmt_foreach->var_name.value.value();
                gen.create_var(user_var_name, deref_expr, stmt_foreach->var_name, std::nullopt);
                
                for(const auto& st : stmt_foreach->scope->stmts) {
                    gen.gen_stmt(st);
                }
                
                auto* it_expr_inc = gen.make_ident_expr(it_name);
                auto* one_expr = gen.make_int_lit(1);
                auto* inc_stmt = gen.make_inc_stmt(it_expr_inc, one_expr, stmt_foreach->var_name);
                gen.gen_stmt(inc_stmt);
                
                gen.m_builder.jmp(gen.label(start_label));
                gen.m_builder.label(end_label);
                
                gen.m_breaks.pop_back();
                gen.m_break_scopes.pop_back();
                
                gen.end_scope();
            }
            void operator()(const NodeStmtEnum* stmt_enum) const
            {
                GString ns_name = stmt_enum->name;
                Namespace* nm;
                
                auto it = gen.m_namespaces.find(ns_name);
                if (it == gen.m_namespaces.end()) {
                    nm = gen.m_allocator->emplace<Namespace>();
                    nm->name = ns_name;
                    nm->def = stmt_enum->def;
                    nm->procs = {};
                    nm->consts = {}; 
                    gen.m_namespaces[ns_name] = nm;
                } else {
                    nm = it->second;
                }
                
                for (const auto& p : stmt_enum->members) {
                    if (nm->consts.find(p.first) != nm->consts.end()) {
                        gen.GeneratorError(stmt_enum->def, "constant `" + p.first + "` already defined in enum/namespace `" + ns_name + "`");
                    }
                    nm->consts[p.first] = p.second;
                }
                
                if (gen.struct_lookup(ns_name).has_value() == false) {
                    // TODO: define enum type
                }
            }
        };

        StmtVisitor visitor{ *this };
        std::visit(visitor, stmt->var);
    }

    [[nodiscard]] GString gen_prog()
    {
        m_consts.push_back({});
        m_typedefs.push_back({});
    
        if (!m_string_index)      m_string_index      = static_cast<size_t*>(malloc(sizeof(size_t)));
        if (!m_label_count)       m_label_count       = static_cast<size_t*>(malloc(sizeof(size_t)));
        if (!m_typeid_table_size) m_typeid_table_size = static_cast<size_t*>(malloc(sizeof(size_t)));
    
        *m_typeid_table_size = 5ULL;
        *m_label_count       = 0ULL;
        *m_string_index      = 0ULL;
    
        IRProgram main_ir;
        IRProgram templ_ir;
    
        m_main_ir  = &main_ir;
        m_templ_ir = &templ_ir;
        m_builder  = IRBuilder(m_main_ir);

        GVector<PendingTemplateInstance> pending;
        m_pending_templates = &pending;
    
        m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("public __BpmDoubleExceptionTypeId")));
        m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("public __BpmRecursionExceptionTypeId")));
        m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("public __BpmSigSegvExceptionTypeId")));
    
        for (const NodeStmt* stmt : m_prog->stmts) {
            gen_stmt(stmt);
        }

        gen_all_template_instances();

        m_builder.label("main");
        m_builder.push( Operand::regOp(Reg::EBP) );

        m_builder.mov(
            Operand::regOp(Reg::EBP),
            Operand::regOp(Reg::ESP)
        );

        m_builder.push( Operand::memOp( MemRef::baseDisp(Reg::EBP, 12) ) );
        m_builder.push( Operand::memOp( MemRef::baseDisp(Reg::EBP, 8) ) );

        m_builder.call(sym("__bpm_set_sigsegv_handler"));
        m_builder.push(reg(Reg::EBP));
        m_builder.call(sym("gc_set_stack_base"));
        m_builder.add(reg(Reg::ESP), imm(4));

        m_builder.mov(mem(MemRef::sym("__BpmDoubleExceptionTypeId")),   imm(0));
        m_builder.mov(mem(MemRef::sym("__BpmRecursionExceptionTypeId")), imm(0));
        m_builder.mov(mem(MemRef::sym("__BpmSigSegvExceptionTypeId")),  imm(0));

        m_builder.push(imm(static_cast<int32_t>((*m_typeid_table_size) * 4ULL)));
        m_builder.call(sym("malloc"));
        m_builder.add(reg(Reg::ESP), imm(4));
        m_builder.mov(mem(MemRef::sym("__type_id_table")), reg(Reg::EAX));

        m_init_typeid_table_ir();

        m_builder.call(sym("__main"));
        m_builder.add(reg(Reg::ESP), imm(8));

        m_builder.push(imm(0));
        m_builder.call(sym("ExitProcess"));

        bool has_oninits = !__oninits.empty();

        m_builder.label("_BPM_init_");
        if (has_oninits) {
            m_builder.push(reg(Reg::EBP));
            m_builder.mov(reg(Reg::EBP), reg(Reg::ESP));
        }

        m_builder.mov(mem(MemRef::sym("tmp_p")), imm(0));

        auto init_builtin_id = [&](const GString& struct_name, const GString& global_name) {
            auto it = m_structs.find(struct_name);
            if (it != m_structs.end()) {
                m_builder.mov(
                    mem(MemRef::sym(global_name)),
                    imm(static_cast<int32_t>(it->second.m_typeid))
                );
            }
        };
        init_builtin_id("__DoubleFreeException", "__BpmDoubleExceptionTypeId");
        init_builtin_id("__RecursionException",  "__BpmRecursionExceptionTypeId");
        init_builtin_id("__SigSegvException",    "__BpmSigSegvExceptionTypeId");

        for (const NodeScope* scope : __oninits) {
            gen_scope(scope);
        }

        if (has_oninits) {
            m_builder.pop(reg(Reg::EBP));
        }
        m_builder.ret();

        IRProgram final_ir;

        final_ir.instrs.insert(final_ir.instrs.end(),
                               m_main_ir->instrs.begin(), m_main_ir->instrs.end());
        final_ir.instrs.insert(final_ir.instrs.end(),
                               m_templ_ir->instrs.begin(), m_templ_ir->instrs.end());
    
        for (auto& ext : m_cexterns)
            final_ir.externs.push_back(ext);
    
        for (auto&& p : m_strings) {
            final_ir.strings.push_back({
                "s_" + GString(std::to_string(p.second.index).c_str()),
                p.second.value
            });
        }
    
        for (auto&& p : m_global_vars) {
            final_ir.globals.push_back({ p.second.name });
        }
        if (m_optimize) {
            iropt::optimize_ir(final_ir);
        }
        GStringStream result_ss;
        AsmEmitter emitter(result_ss);
        emitter.emit_program(final_ir);
        return GString(result_ss.str());
    }

    bool m_optimize = false;

private:
    bool is_interface_type(const DataType& dt) {
        if (!dt.is_object()) return false;
        GString nm = dt.getobjectname();
        return inter_lookup(nm).has_value();
    }

    GString mangle_ns_name(const GString& ns) const {
        GString out;
        out.reserve(ns.size());
        for (char c : ns) {
            if (c == ':') {
                out.push_back('_');
                continue;
            }
            out.push_back(c);
        }
        return out;
    }

    Interface get_interface(const DataType& dt, const Token& where) {
        assert(dt.is_object());
        GString nm = dt.getobjectname();
        auto i = inter_lookup(nm);
        if (!i.has_value()) {
            GeneratorError(where, "type `" + dt.to_string() + "` is not an interface type");
        }
        return i.value();
    }
    GString format_type_list(const GVector<DataType>& types) {
        GString res;
        for (int i = 0; i < static_cast<int>(types.size()); ++i) {
            res += types[i].to_string();
            if (i + 1 < static_cast<int>(types.size())) res += ", ";
        }
        return res;
    }

    bool derive_templates_core(GVector<DataType>& targs,
                               const GVector<std::pair<GString, DataType>>& params,
                               GVector<GString>* templates,
                               const GVector<NodeExpr*>& args,
                               UNUSED_ARG const Procedure& proc,
                               GMap<GString, DataType>& temps_out)
    {
        assert(templates != NULL);
        assert(!templates->empty());

        targs.clear();
        targs.reserve(templates->size());
        for (int i = 0; i < static_cast<int>(templates->size()); ++i) {
            targs.emplace_back(SimpleDataType::_void);
        }

        size_t redo_count = 0;
        GMap<GString, DataType> temps;

        while (true) {
            for (int i = 0; i < static_cast<int>(params.size()); ++i) {
                int  counter   = -1;
                int  temp_s    = -1;
                bool is_temp_s = false;

                for (int j = 0; j < static_cast<int>(templates->size()); ++j) {

                    if (params[i].second.is_object() && 
                        templates->operator[](j) == params[i].second.getobjectname()) 
                    {
                        counter = j;
                        break;
                    }

                    const auto& p_gens = params[i].second.node->generics;
                    temp_s = 0;
                    
                    for(size_t k = 0; k < p_gens.size(); ++k) {
                        GString gen_name = "";
                        if(p_gens[k].is_object()) gen_name = p_gens[k].getobjectname();

                        if (p_gens[k].is_object() && 
                            templates->operator[](j) == p_gens[k].getobjectname()) 
                        {
                            if (targs[j].root().is_simple() &&
                                targs[j].root().getsimpletype() == SimpleDataType::_void)
                            {
                                counter = j;
                                is_temp_s = true;
                                break;
                            }
                        }
                        temp_s++;
                    }
                    
                    if (is_temp_s) break;
                    else temp_s = -1;
                }

                if (counter != -1) {
                    if (targs[counter].root().is_simple() &&
                        targs[counter].root().getsimpletype() == SimpleDataType::_void)
                    {
                        if (is_temp_s) {
                            DataType ct = type_of_expr(args[i]);
                            
                            if (temp_s >= 0 && temp_s < static_cast<int>(ct.node->generics.size())) {
                                targs[counter] = ct.node->generics[temp_s];
                            }
                        } else {
                            DataType arg_tp = type_of_expr(args[i]);
                            arg_tp.root().link   = false;
                            arg_tp.root().rvalue = false;
                            targs[counter]       = arg_tp;
                        }
                    }
                }
            }

            bool all_set = true;
            for (int i = 0; i < static_cast<int>(templates->size()); ++i) {
                if (targs[i].root().is_simple() &&
                    targs[i].root().getsimpletype() == SimpleDataType::_void)
                {
                    all_set = false;
                    break;
                }
            }
            if (all_set) break;

            if (redo_count > 0) {
                return false;
            }
            redo_count++;
        }

        size_t counter = 0;
        for (auto&& el : *templates) {
            temps[el] = targs[counter++];
        }
        temps_out = std::move(temps);
        return true;
    }

    bool implements_interface_quiet(const DataType& ty, const GString& iface_name) {
        if (!ty.is_object()) return false;

        GString tname = ty.getobjectname();

        std::pair<GString, GString> key{ tname, iface_name };

        auto it_cache = m_impl_cache.find(key);
        if (it_cache != m_impl_cache.end()) {
            return it_cache->second;
        }

        if (tname == iface_name) {
            m_impl_cache[key] = true;
            return true;
        }

        auto ifcOpt = inter_lookup(iface_name);
        if (!ifcOpt.has_value()) {
            m_impl_cache[key] = false;
            return false;
        }
        const Interface& ifc = ifcOpt.value();

        auto nmsOpt = namespace_lookup(tname);
        if (!nmsOpt.has_value()) {
            m_impl_cache[key] = false;
            return false;
        }
        Namespace* ns = nmsOpt.value();

        for (const auto& km : ifc.methods) {
            const GString& mname      = km.first;
            const InterfaceMethodInfo& im = km.second;

            const auto it = ns->procs.find(mname);
            if (it == ns->procs.end()) {
                m_impl_cache[key] = false;
                return false;
            }
            const Procedure& baseProc = it->second;

            bool ok = method_matches_interface(baseProc, im, ty);
            if (!ok) {
                for (Procedure* ov : baseProc.overrides) {
                    if (method_matches_interface(*ov, im, ty)) {
                        ok = true;
                        break;
                    }
                }
            }
            if (!ok) {
                m_impl_cache[key] = false;
                return false;
            }
        }

        m_impl_cache[key] = true;
        return true;
    }

    bool can_convert(const DataType& from, const DataType& to) {
        DataType fc = canonical_type(from);
        DataType tc = canonical_type(to);

        if (fc == tc) return true;
        if (is_base_of(tc, fc)) return true;

        if (is_interface_type(tc)) {
            if (is_interface_type(fc)) {
                return fc.getobjectname() == tc.getobjectname();
            }
            if (fc.is_object()) {
                return implements_interface_quiet(fc, tc.getobjectname());
            }
            return false;
        }

        if (fc.root().arg_eq(tc.root())) {
             if (fc.node->generics.empty() && tc.node->generics.empty()) return true;
             
             if (fc.is_simple() && fc.root().getsimpletype() == SimpleDataType::proc_ptr) return false;
             
             return true; 
        }

        if (tc.root().is_simple() && tc.root().getsimpletype() == SimpleDataType::any) return true;

        return false;
    }

    bool match_call_signature(const GVector<NodeExpr*>& args,
                              const GVector<std::pair<GString, DataType>>& params,
                              UNUSED_ARG const Procedure& proc,
                              bool nosizedargs,
                              int* bad_index /*может быть nullptr*/)
    {
        if (args.size() != params.size() && !nosizedargs) return false;
        if (args.size() <  params.size() &&  nosizedargs) return false;

        for (int i = 0; i < static_cast<int>(params.size()); ++i) {
            DataType arg_raw = type_of_expr(args[i]);
            const DataType& ex_raw = params[i].second;
            DataType argtype = canonical_type(arg_raw);
            DataType ex_type = canonical_type(ex_raw);

            bool used_conversion = false;

            if (!argtype.is_compatible_with(ex_type)) {
                if (!can_convert(argtype, ex_type)) {
                    if (bad_index) *bad_index = i;
                    return false;
                }
                used_conversion = true;
            }

            if (!used_conversion && argtype.is_object() && ex_type.is_object()) {
                auto arg_targs = get_template_args(argtype);
                auto ex_targs  = get_template_args(ex_type);
                if (arg_targs.size() != ex_targs.size()) {
                    if (bad_index) *bad_index = i;
                    return false;
                }
                for (size_t j = 0; j < arg_targs.size(); ++j) {
                    if (arg_targs[j] != ex_targs[j]) {
                        if (bad_index) *bad_index = i;
                        return false;
                    }
                }
            }
        }
        return true;
    }

    bool method_matches_interface(const Procedure& impl,
                              const InterfaceMethodInfo& iface,
                              const DataType& selfType)
    {
        DataType selfCanon = canonical_type(selfType);

        Procedure inst = impl;
        if (selfType.is_object()) {
            GString oname = selfType.getobjectname();
            std::optional<Struct> stOpt = struct_lookup(oname);
            if (stOpt.has_value() && stOpt.value().temp) {
                GVector<DataType> targs = get_template_args(selfType);
                const Struct& st = stOpt.value();

                if (targs.size() == st.temps.size()) {
                    GMap<GString, DataType> temps;
                    for (size_t i = 0; i < st.temps.size(); ++i) {
                        temps[st.temps[i]] = targs[i];
                    }
                    for (auto& p : inst.params) {
                        substitute_template_wct(p.second, temps);
                    }
                    substitute_template_wct(inst.rettype, temps);
                }
            }
        }

        if (inst.params.size() != iface.params.size())
            return false;

        for (size_t i = 0; i < iface.params.size(); ++i) {
            DataType exp = canonical_type(iface.params[i].second);
            DataType act = canonical_type(inst.params[i].second);

            if (exp.is_object() && exp.getobjectname() == "self") {
                if (act != selfCanon) return false;
            } else {
                if (exp != act) return false;
            }
        }

        DataType expRet = canonical_type(iface.rettype);
        DataType actRet = canonical_type(inst.rettype);

        if (expRet.is_object() && expRet.getobjectname() == "self") {
            if (actRet != selfCanon) return false;
        } else {
            if (expRet != actRet) return false;
        }

        return true;
    }

    bool check_implements_quiet(const DataType& ty, const DataType& iface_type) {
        GString iface_name = iface_type.getobjectname();
        
        if (iface_name == "__ObjectTypeI") return ty.is_object();
        if (iface_name == "__SimpleTypeI") return ty.is_simple();
        if (iface_name == "__PointerTypeI") return (ty.root().ptrlvl > 0 || (ty.is_simple() && ty.root().getsimpletype() == SimpleDataType::ptr));

        if (!ty.is_object()) return false;

        GString tname = ty.getobjectname();
        auto ifcOpt = inter_lookup(iface_name);
        if (!ifcOpt.has_value()) return false;
        const Interface& ifc = ifcOpt.value();

        GMap<GString, DataType> iface_temps;
        auto& args = iface_type.node->generics;
        if (args.size() != ifc.temps.size()) return false;
        for(size_t i=0; i<args.size(); ++i) iface_temps[ifc.temps[i]] = args[i];

        auto nmsOpt = namespace_lookup(tname);
        if (!nmsOpt.has_value()) return false;
        Namespace* ns = nmsOpt.value();

        for (const auto& km : ifc.methods) {
            InterfaceMethodInfo im = km.second;
            for(auto& p : im.params) substitute_template_wct(p.second, iface_temps);
            substitute_template_wct(im.rettype, iface_temps);

            const auto it = ns->procs.find(km.first);
            if (it == ns->procs.end()) return false;
            const Procedure& baseProc = it->second;

            bool ok = method_matches_interface(baseProc, im, ty);
            if (!ok) {
                for (Procedure* ov : baseProc.overrides) {
                    if (method_matches_interface(*ov, im, ty)) {
                        ok = true;
                        break;
                    }
                }
            }
            if (!ok) return false;
        }
        return true;
    }

    void ensure_implements_interface(const DataType& ty,
                                 const DataType& iface_type,
                                 const Token& where)
    {
        if (!ty.is_object()) {
            GeneratorError(where,
                "type " + ty.to_string() +
                " is not an object type and cannot implement interface `" +
                iface_type.to_string() + "`");
        }

        GString iface_name = iface_type.getobjectname();
        GString tname = ty.getobjectname();

        if (iface_name == "__ObjectTypeI") {
            if (ty.is_object()) return;
            GeneratorError(where, "type `" + ty.to_string() + "` is not an object type");
        }
        
        if (iface_name == "__SimpleTypeI") {
            if (ty.is_simple()) return;
            GeneratorError(where, "type `" + ty.to_string() + "` is not a simple type");
        }
        
        if (iface_name == "__PointerTypeI") {
            if (ty.root().ptrlvl > 0 || (ty.is_simple() && ty.root().getsimpletype() == SimpleDataType::ptr)) return;
            GeneratorError(where, "type `" + ty.to_string() + "` is not a pointer");
        }
        
        std::optional<Interface> ifcOpt = inter_lookup(iface_name);
        if (!ifcOpt.has_value()) {
            GeneratorError(where, "unkown interface `" + iface_name + "`");
        }
        const Interface& ifc = ifcOpt.value();

        GMap<GString, DataType> iface_temps;
        auto& args = iface_type.node->generics;
        
        if (args.size() != ifc.temps.size()) {
             GeneratorError(where, "interface `" + iface_name + "` expects " + 
                            GString(std::to_string(ifc.temps.size()).c_str()) + " template args, but got " + 
                            GString(std::to_string(args.size()).c_str()));
        }
        
        for(size_t i=0; i<args.size(); ++i) {
            iface_temps[ifc.temps[i]] = args[i];
        }

        std::optional<Namespace*> nmsOpt = namespace_lookup(tname);
        if (!nmsOpt.has_value()) {
            GeneratorError(where,
                "type `" + tname + "` does not implement interface `" +
                iface_type.to_string() + "`: no `impl " + tname + " { ... }` with required methods");
        }
        Namespace* ns = nmsOpt.value();

        for (const auto& km : ifc.methods) {
            const GString& mname = km.first;
            InterfaceMethodInfo im = km.second; 
            
            for(auto& p : im.params) substitute_template_wct(p.second, iface_temps);
            substitute_template_wct(im.rettype, iface_temps);

            const auto it = ns->procs.find(mname);
            if (it == ns->procs.end()) {
                GeneratorError(where,
                    "type `" + tname + "` does not implement interface `" +
                    iface_type.to_string() + "`: missing method `" + mname + "`");
            }
            const Procedure& baseProc = it->second;

            bool ok = method_matches_interface(baseProc, im, ty);
            if (!ok) {
                for (Procedure* ov : baseProc.overrides) {
                    if (method_matches_interface(*ov, im, ty)) {
                        ok = true;
                        break;
                    }
                }
            }

            if (!ok) {
                GeneratorError(where,
                    "type `" + tname + "` does not correctly implement interface `" +
                    iface_type.to_string() + "`: method `" + mname + "` has incompatible signature");
            }
        }
    }

        void gen_box_interface(const DataType& ifaceType,
                       const DataType& srcType,
                       const Token& where)
    {
        Interface iface = get_interface(ifaceType, where);
        size_t methods_count = iface.method_order.size();

        GMap<GString, DataType> iface_temps;
        auto& args = ifaceType.node->generics;
        
        if (args.size() != iface.temps.size()) {
             GeneratorError(where, "interface `" + iface.name + "` expects " + 
                            GString(std::to_string(iface.temps.size()).c_str()) + " template args, but got " + 
                            GString(std::to_string(args.size())).c_str());
        }
        
        for(size_t i=0; i<args.size(); ++i) {
            iface_temps[iface.temps[i]] = args[i];
        }

        size_t wrapper_bytes = (1 + methods_count) * 4;

        pop_reg(Reg::EAX); 
        push_reg(Reg::EAX);

        push_imm(static_cast<int32_t>(wrapper_bytes));
        m_builder.call(sym("memalloc"));
        m_builder.add(reg(Reg::ESP), imm(4));

        pop_reg(Reg::ECX);
        m_builder.mov(reg(Reg::EDX), reg(Reg::EAX));
        m_builder.mov(mem(MemRef::baseDisp(Reg::EDX, 0)),
                      reg(Reg::ECX));

        GString tname = srcType.getobjectname();
        auto nms = namespace_lookup(tname);
        if (!nms.has_value()) {
            GeneratorError(where, "type `" + tname +
                                  "` has no namespace for dynamic interface");
        }
        Namespace* ns = nms.value();

        for (size_t i = 0; i < methods_count; ++i) {
            const GString& mname = iface.method_order[i];
            auto it = ns->procs.find(mname);
            if (it == ns->procs.end()) {
                GeneratorError(where,
                    "type `" + tname + "` does not implement method `" + mname +
                    "` dynamically");
            }
            Procedure proc = it->second;

            GString tsign;

            if (proc.templates != NULL) {
                GVector<DataType> local_targs = get_template_args(srcType);
                GVector<NodeExpr*> empty_args;
                
                auto inst_res = instantiate_if_needed(proc, local_targs, empty_args,
                                              where, mname, tname);
                tsign = inst_res.first;
            }
            
            GString label = tname + "@" + mname + tsign;
            if (proc.override) label += proc.get_sign();
            MemRef dst_mem = MemRef::baseDisp(Reg::EDX,
                                              static_cast<int32_t>((1 + i) * 4));
            m_builder.mov(mem(dst_mem), sym(label));
        }
        push_reg(Reg::EDX);
    }

    std::pair<GString, GMap<GString, DataType>> instantiate_if_needed(
        Procedure& proc,
        GVector<DataType>& local_targs,
        const GVector<NodeExpr*>& call_args,
        const Token& def,
        const GString& name,
        const GString& nname /* = "" */
    ) {
        bool is_method_of_struct  = false;
        Struct ownerStruct;
        size_t structTemplateCount  = 0;

        bool explicit_inst = !local_targs.empty() && call_args.empty();

        if (!nname.empty() && !explicit_inst) {
            std::optional<Struct> stOpt = struct_lookup(nname);
            if (stOpt.has_value() && stOpt->temp && !proc.params.empty()) {
                DataType selfParamType = canonical_type(proc.params[0].second);
                if (selfParamType.is_object() &&
                    selfParamType.getobjectname() == nname &&
                    proc.templates != nullptr &&
                    proc.templates->size() >= stOpt->temps.size())
                {
                    is_method_of_struct  = true;
                    ownerStruct          = stOpt.value();
                    structTemplateCount  = ownerStruct.temps.size();
                }
            }
        }

        GMap<GString, DataType> temps;
        GString                  tsign;

        if (is_method_of_struct) {
            if (call_args.empty()) {
                GeneratorError(def, "internal compiler error: method call without self argument");
            }

            DataType selfType = type_of_expr(call_args[0]);
            substitute_template(selfType);
            selfType = canonical_type(selfType);

            if (!selfType.is_object()) {
                GeneratorError(def, "method `" + proc.name + "` called on non-object self type " +
                                     selfType.to_string());
            }

            GString oname = selfType.getobjectname();
            if (oname != nname) {
                GeneratorError(def, "method `" + proc.name + "` called with self of different type: expected `" +
                                     nname + "`, got `" + oname + "`");
            }

            GVector<DataType> structArgs = get_template_args(selfType);
            if (structArgs.size() != structTemplateCount) {
                GeneratorError(def, "internal compiler error: mismatch struct template args count for `" +
                                     nname + "`");
            }

            for (size_t i = 0; i < structTemplateCount; ++i) {
                temps[ownerStruct.temps[i]] = structArgs[i];
            }

            for (auto& t : structArgs) {
                DataType tt = t;
                substitute_template(tt);
                tsign += tt.sign();
            }
        } else {
            if (proc.templates != NULL && local_targs.empty()) {
                if (!call_args.empty() || proc.params.empty()) {
                    try_derive_templates(local_targs, proc.params, def, proc.templates, call_args, proc);
                }
            }

            if (proc.templates != NULL && local_targs.empty()) {
                GeneratorError(def,
                    "procedure `" + proc.name +
                    "` expects template arguments in <...> or successful type deduction.");
            }

            if (local_targs.empty())
                return {"", temps}; 

            if (proc.templates != NULL && local_targs.size() != proc.templates->size()) {
                GeneratorError(def, "template args mismatch");
            }

            size_t counter = 0;
            for (auto&& el : *proc.templates) {
                DataType arg_dt = local_targs[counter++];
                substitute_template(arg_dt);
                temps[el] = arg_dt;
            }


            for (int i = 0; i < static_cast<int>(local_targs.size()); ++i) {
                DataType t = local_targs[i];
                substitute_template(t);
                tsign += t.sign();
            }
        }

        if (proc.from != nullptr && !proc.from->constraints.empty()) {
            for (const auto& c : proc.from->constraints) {
                auto it = temps.find(c.type_param);
                if (it == temps.end()) {
                    continue;
                }
                DataType actual = it->second;

                DataType required_iface = c.iface_type;
                substitute_template_wct(required_iface, temps);
                
                ensure_implements_interface(actual, required_iface, def);
            }
        }

        Procedure* inst_p = nullptr;
        if (nname.empty()) {
            if (proc.override)
                inst_p = m_procs[name].overrides[proc.overload_nth - 1];
            else
                inst_p = &m_procs[name];
        } else {
            if (proc.override)
                inst_p = m_namespaces[nname]->procs[name].overrides[proc.overload_nth - 1];
            else
                inst_p = &m_namespaces[nname]->procs[name];
        }

        if (!inst_p->instanceated[tsign]) {
            inst_p->instanceated[tsign] = true;

            PendingTemplateInstance pt;
            pt.proc  = proc; 
            pt.tsign = tsign;
            pt.nname = nname;
            pt.temps = temps;

            if (!m_pending_templates) {
                GeneratorError(def, "internal error: m_pending_templates is null");
            }

            m_pending_templates->push_back(std::move(pt));
        }

        return {tsign, temps};
    }

    struct PendingTemplateInstance {
        Procedure                    proc;  
        GString                  tsign; 
        GString                  nname; 
        GMap<GString, DataType> temps; 
    };

    GVector<PendingTemplateInstance>* m_pending_templates = nullptr;

    void gen_template_instance(const PendingTemplateInstance& pt) {
        IRBuilder old_builder = m_builder;
        m_builder = IRBuilder(m_templ_ir);

        m_temps.push_back(pt.temps);

        Procedure proc          = pt.proc;
        const GString& tsign = pt.tsign;
        const GString& nname = pt.nname;

        GString lbl;
        if (nname.empty()) lbl = proc.name + tsign;
        else               lbl = nname + "@" + proc.name + tsign;
        if (proc.override) lbl += proc.get_sign();

        m_builder.label(lbl);
        m_builder.push(reg(Reg::EBP));
        m_builder.mov(reg(Reg::EBP), reg(Reg::ESP));

        if (nname.empty())
            gen_traceback_push(proc);
        else
            gen_traceback_push_nm(proc, nname);
        m_builder.call(sym("__bpm_proc_enter"));

        m_tsigns.push_back(tsign);
        if (!nname.empty()) proc.mbn = nname;

        m_cur_proc = proc;
        gen_scope_sp(proc.scope, proc.from, proc);

        push_reg(Reg::EAX);
        m_builder.call(sym("__bpm_proc_leave"));
        m_builder.call(sym("traceback_pop"));
        pop_reg(Reg::EAX);
        m_builder.pop(reg(Reg::EBP));
        m_builder.ret();

        m_temps.pop_back();
        m_tsigns.pop_back();

        m_builder = old_builder;
    }

    void gen_all_template_instances() {
        for (size_t i = 0; i < m_pending_templates->size(); ++i) {
            gen_template_instance((*m_pending_templates)[i]);
        }
    }

    void m_init_typeid_table_ir() {
        m_builder.mov(
            reg(Reg::ECX),
            mem(MemRef::sym("__type_id_table"))
        );

        for (auto&& tp : m_typeid_table) {
            size_t _index;
            __str_ref value = tp.second;
            std::optional<String> str = string_lookup(value);

            if (!str.has_value()) {
                size_t index = (*m_string_index)++;
                m_strings[value] = { value, index };
                _index = index;
            } else {
                _index = str.value().index;
            }

            m_builder.mov(
                mem(MemRef::baseDisp(Reg::ECX, static_cast<int32_t>(tp.first * 4ULL))),
                sym("s_" + GString(std::to_string(_index).c_str()))
            );
        }
    }

    void begin_scope(int fsz)
    {
        if (fsz != 0) {
            m_builder.sub(reg(Reg::ESP), imm(static_cast<int32_t>(fsz * 4)));
        }
        m_consts.push_back({});
        m_vars.push_back({});
        m_typedefs.push_back({});
        m_scopes_vi.push_back(m_var_index);
        m_scopes.push_back(fsz);
    }

    void end_scope()
    {
        if (m_scopes[m_scopes.size() - 1ULL] != 0ULL) {
            m_builder.add(
                reg(Reg::ESP),
                imm(static_cast<int32_t>(m_scopes[m_scopes.size() - 1ULL] * 4))
            );
        }
        m_scopes.pop_back();
        m_vars.pop_back();
        m_consts.pop_back();
        m_typedefs.pop_back();
        m_var_index = m_scopes_vi[m_scopes_vi.size() - 1ULL];
        m_scopes_vi.pop_back();
    }

    void end_scope_sp(Procedure& proc, UNUSED_ARG const Token& def)
    {
        GString lbl;
        if (in_namespace()) lbl += "__" + mangle_ns_name(m_cur_namespace->name) + "@";
        else                lbl += "__";
        if (!proc.mbn.empty()) lbl += proc.mbn + "@";
        lbl += proc.name;
        if (!m_tsigns.empty()) lbl += m_tsigns.back();
        if (proc.override)      lbl += proc.get_sign();
        lbl += "@ret";

        m_builder.label(lbl);

        if (m_scopes[m_scopes.size() - 1ULL] != 0ULL) {
            m_builder.add(
                reg(Reg::ESP),
                imm(static_cast<int32_t>(m_scopes[m_scopes.size() - 1ULL] * 4))
            );
        }
        m_scopes.pop_back();
        m_vars.pop_back();
        m_consts.pop_back();
        m_typedefs.pop_back();
        m_var_index = m_scopes_vi[m_scopes_vi.size() - 1ULL];
        m_scopes_vi.pop_back();
    }

    inline size_t __compute_allign_ret() noexcept {
        size_t res = 0ULL;
        for (size_t i = 0; i < m_scopes.size(); ++i) {
            if (i == 0) continue;
            res += m_scopes[i];
        }
        return res;
    }

    inline GString create_label() noexcept
    {
        return ".L" + GString(std::to_string((*m_label_count)++).c_str());
    }

    const NodeProg* m_prog = nullptr;

    IRProgram* m_main_ir  = nullptr;
    IRProgram* m_templ_ir = nullptr;
    
    IRBuilder m_builder{ nullptr };

    DiagnosticManager* m_diag_man = nullptr;

    GVector<GMap<GString, Var>> m_vars;
    GMap<GString, String>        m_strings;
    GMap<GString, Procedure>     m_procs;
    GMap<GString, Struct>        m_structs;
    GMap<GString, GVar>          m_global_vars;
    GMap<GString, Interface>     m_interfaces;
    GMap<GString, Namespace*>    m_namespaces;

    GVector<GString> m_ns_stack;

    Namespace* m_cur_namespace = nullptr;
    ArenaAllocator* m_allocator = nullptr;

    size_t m_structs_count = 5;
    std::optional<Procedure> m_cur_proc;

    GVector<GString> m_breaks;
    VectorSim<size_t>        m_scopes;
    VectorSim<size_t>        m_scopes_vi;
    VectorSim<size_t>        m_break_scopes;

    GSet<GString> m_used_labels;

    GVector<GMap<GString, Constant>> m_consts;
    GVector<GMap<GString, DataType>> m_typedefs;

    GVector<std::pair<size_t, GString>> m_typeid_table{
        {TYPEID_INT,  "int"},
        {TYPEID_PTR,  "ptr"},
        {TYPEID_VOID, "void"},
        {TYPEID_ANY,  "any"},
        {TYPEID_CHAR, "char"},
    };

    GVector<GString> m_cexterns{
        "ExitProcess",
        "malloc",
        "free",
        "memcpy",
        "memalloc",
        "memfree",
        "gc_collect",
        "gc_set_stack_base",
        "__bpm_gc_dump_state",
        "__current_exception",
        "__bpm_exception_throwed",
        "__bpm_start_catch",
        "__bpm_terminate",
        "__bpm_allocate_exception",
        "__bpm_throw",
        "__type_id_table",
        "_setjmp",
        "longjmp",
        "__bpm_end_catch",
        "__exception_bufs",
        "__exception_bufs_lvl",
        "__bpm_get_current_exception",
        "traceback_push",
        "traceback_pop",
        "__bpm_set_sigsegv_handler",
        "__sigsegv_wh_exception",
        "__bpm_double_free_exception_what",
        "traceback",
        "__bpm_recursion_exception_what",
        "__traceback_saved",
        "__bpm_proc_enter",
        "__bpm_proc_leave",
    };

    GVector<NodeScope*>                __oninits;
    GVector<GString>               m_tsigns;
    GVector<GMap<GString, DataType>> m_temps;
    GSet<std::pair<GString,GString>> m_dyn_iface_impls;
    GMap<std::pair<GString, GString>, bool> m_impl_cache;

    size_t* m_string_index      = nullptr;
    size_t  CTX_IOTA            = 0ULL;
    size_t  m_var_index         = 0ULL;
    size_t* m_label_count       = nullptr;
    size_t* m_typeid_table_size = nullptr;
};