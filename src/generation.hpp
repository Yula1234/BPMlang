#pragma once

#include "parser.hpp"
#include "ir.hpp"
#include "ir_opt.hpp"

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

using __str_ref = const std::string&;

void consume_un(...) {}

namespace INTERNAL_CODE {
    std::string IMPLEMENTATION = R"(struct exception { __message: char*, __bstub1: int, __bstub2: int }
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
namespace std { proc exception(char* mess_) -> exception = return exception(mess_, 0, 0); })";
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

class Generator {
public:
    inline Operand reg(Reg r) const              { return Operand::regOp(r); }
    inline Operand imm(int32_t v) const          { return Operand::immOp(v); }
    inline Operand label(const std::string& s) const { return Operand::labelOp(s); }
    inline Operand sym(const std::string& s) const   { return Operand::symbolOp(s); }
    inline Operand mem(const MemRef& m) const    { return Operand::memOp(m); }

    inline MemRef local_mem(size_t stack_loc) const {
        return MemRef::baseDisp(Reg::EBP, -static_cast<int32_t>(stack_loc));
    }
    inline MemRef global_mem(const std::string& name) const {
        return MemRef::sym("v_" + name);
    }

    inline void push_reg(Reg r)                { m_builder.push(reg(r)); }
    inline void push_imm(int32_t v)            { m_builder.push(imm(v)); }
    inline void push_mem(const MemRef& m)      { m_builder.push(mem(m)); }
    inline void push_sym(const std::string& s) { m_builder.push(sym(s)); }

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
            std::string sname = type.root().getobjectname();
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
            std::string name = type.root().getobjectname();
            std::optional<Struct> st = struct_lookup(name);
            if (st.has_value()) {
                return st.value().fields.size() * 4ULL;
            }
            std::optional<Interface> inter = inter_lookup(name);
            if (inter.has_value()) {
                return inter.value().fields.size() * 4ULL;
            }
            return 0ULL;
        } else {
            return 4ULL;
        }
    }

    struct Var {
        std::string name{};
        size_t      stack_loc{};
        DataType    type;

        std::string ref() {
            return "dword [ebp-" + std::to_string(stack_loc) + "]";
        }
        MemRef mem() const {
            return MemRef::baseDisp(Reg::EBP, -static_cast<int32_t>(stack_loc));
        }
    };

    struct GVar {
        std::string name;
        DataType    type;
    };

    struct Procedure {
        std::string name{};
        std::vector<std::pair<std::string, DataType>> params{};
        DataType rettype;
        size_t   stack_allign;
        std::vector<ProcAttr> attrs;
        Token    def;
        bool     prototype;
        std::vector<Procedure*> overrides;
        bool     override;
        std::optional<int> uniq_sign;
        __stdvec<std::string>* templates;
        const NodeScope*  scope;
        const NodeStmtProc* from;
        __map<std::string, bool> instanceated;
        std::string mbn;
        int overload_nth = 0;

        std::string get_sign() {
            if (params.empty()) {
                if (!uniq_sign.has_value()) uniq_sign = rand() % 1000;
                return std::to_string(uniq_sign.value());
            }
            std::string res;
            for (size_t i = 0; i < params.size(); ++i) {
                res += params[i].second.sign();
            }
            return res;
        }

        void gen_ret(Generator& gen, std::optional<std::string> cnm) {
            size_t allign = gen.__compute_allign_ret();
            if (allign != 0) {
                gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(allign * 4));
            }
            std::string lbl = "__";
            if (cnm.has_value()) {
                lbl += cnm.value() + "@";
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
        std::string value{};
        size_t      index{};
    };

    struct Struct {
        std::string name;
        __map<std::string, Field> fields;
        std::optional<std::string> __allocator;
        __stdvec<std::pair<std::string, DataType>> __fields;
        size_t  m_typeid;
        bool    temp;
        __stdvec<std::string> temps;
        Token   def;

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

        void call_dtor(Generator& gen, const std::string& offset, const Token& def) const {
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

    struct Interface {
        std::string name;
        __map<std::string, Field> fields;
        std::vector<std::pair<std::string, DataType>> __fields;
        size_t m_typeid;
        inline static bool match_to(const Interface& in, const Struct& st) {
            return in.fields == st.fields;
        }
    };

    struct Namespace {
        __map<std::string, Procedure> procs;
        std::string name;
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
            if (std::holds_alternative<NodeTerm*>(expr->var)) {
                auto term = std::get<NodeTerm*>(expr->var);
                if (std::holds_alternative<NodeTermIntLit*>(term->var)) {
                    return std::stoul(std::get<NodeTermIntLit*>(term->var)->int_lit.value.value());
                }
            }
            gen.GeneratorError(where, "procedure is not constant-evaluatable.");
            return 0;
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
        __stdvec<__map<std::string, int>> m_vars;
        Token where;
        Generator& gen;
    };

    explicit Generator(NodeProg* prog)
        : m_prog(prog), m_allocator(4 * 1024 * 1024) {}

    Generator(const Generator& other)
    : m_allocator(12 * 1024)
	{
	    m_typeid_table   = other.m_typeid_table;
	    m_consts         = other.m_consts;
	    m_typedefs       = other.m_typedefs;
	    m_string_index   = other.m_string_index;
	    m_prog           = other.m_prog;
	    m_lines          = other.m_lines;
	    m_strings        = other.m_strings;
	    m_procs          = other.m_procs;
	    m_structs        = other.m_structs;
	    m_global_vars    = other.m_global_vars;
	    m_interfaces     = other.m_interfaces;
	    m_used_labels    = other.m_used_labels;
	    m_namespaces     = other.m_namespaces;
	    m_cur_namespace  = NULL;
	    m_parser         = other.m_parser;
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
        __map<std::string, Var>& vrs = last_scope();
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

    __map<std::string, Var>& last_scope() {
        return m_vars[m_vars.size() - 1ULL];
    }

    __map<std::string, Constant>& last_scope_cns() {
        return m_consts[m_consts.size() - 1ULL];
    }

    __map<std::string, DataType>& last_scope_tdef() {
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

    std::optional<Field> field_lookup(const Interface& st, __str_ref field) const noexcept {
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
        m_parser->DiagnosticMessage(tok, header, msg, col_inc);
    }

    void GeneratorError(const Token& tok, __str_ref msg) {
        DiagnosticMessage(tok, "error", msg, 0);
        exit(EXIT_FAILURE);
    }

    void GeneratorWarning(const Token& tok, __str_ref msg) {
        DiagnosticMessage(tok, "warning", msg, 0);
    }

    DataType extract_full_type(TreeNode<BaseDataType>* start_node) {
        DataType dt;
        if (!start_node) return dt;
        dt = start_node->data;
        if (start_node->right) {
            TreeNode<BaseDataType>* src  = start_node->right;
            TreeNode<BaseDataType>* dest = dt.list.get_root();
            while (src != nullptr) {
                dt.list.insert_right(src->data, dest);
                dest = dest->right;
                src  = src->right;
            }
        }
        return dt;
    }

    DataType create_datatype_from_chain(TreeNode<BaseDataType>* start_node) {
        DataType dt;
        if (!start_node) return dt;
        dt.list.insert_data(start_node->data, dt.list.get_root_ptr());
        TreeNode<BaseDataType>* src  = start_node->right;
        TreeNode<BaseDataType>* dest = dt.list.get_root();
        while (src != nullptr) {
            dt.list.insert_right(src->data, dest);
            dest = dest->right;
            src  = src->right;
        }
        return dt;
    }

    void append_type_chain(DataType& target_dt, TreeNode<BaseDataType>* target_node, DataType& source_dt) {
        TreeNode<BaseDataType>* src_curr = source_dt.list.get_root();
        TreeNode<BaseDataType>* dst_curr = target_node;
        while (src_curr != nullptr) {
            target_dt.list.insert_right(src_curr->data, dst_curr);
            dst_curr = dst_curr->right;
            src_curr = src_curr->right;
        }
    }

    size_t count_template_nodes(TreeNode<BaseDataType>* start_node) {
        if (!start_node) return 0;
        size_t total_nodes = 1;
        if (start_node->data.is_object) {
            std::string name = start_node->data.getobjectname();
            size_t expected_args = 0;
            std::optional<Struct> st = struct_lookup(name);
            if (st.has_value() && st.value().temp) {
                expected_args = st.value().temps.size();
            }
            TreeNode<BaseDataType>* child_iter = start_node->right;
            for (size_t i = 0; i < expected_args; ++i) {
                if (!child_iter) break;
                size_t arg_len = count_template_nodes(child_iter);
                total_nodes += arg_len;
                for (size_t k = 0; k < arg_len; ++k) {
                    if (child_iter) child_iter = child_iter->right;
                }
            }
        }
        return total_nodes;
    }

    DataType extract_type_nodes(TreeNode<BaseDataType>* start_node, size_t count) {
        DataType dt;
        if (count == 0 || !start_node) return dt;
        dt = start_node->data;
        TreeNode<BaseDataType>* src = start_node->right;
        TreeNode<BaseDataType>* dst = dt.list.get_root();
        for (size_t i = 1; i < count; ++i) {
            if (!src) break;
            dt.list.insert_right(src->data, dst);
            dst = dst->right;
            src = src->right;
        }
        return dt;
    }

    std::vector<DataType> get_template_args(const DataType& dt) {
        std::vector<DataType> res;
        if (!dt.is_object()) return res;
        std::string name = dt.getobjectname();
        std::optional<Struct> st = struct_lookup(name);
        if (!st.has_value() || !st.value().temp) return res;
        TreeNode<BaseDataType>* current = dt.list.get_root()->right;
        for (size_t i = 0; i < st.value().temps.size(); ++i) {
            if (!current) break;
            size_t len = count_template_nodes(current);
            res.push_back(extract_type_nodes(current, len));
            for (size_t k = 0; k < len && current; ++k) {
                current = current->right;
            }
        }
        return res;
    }

    DataType canonical_type(const DataType& src) {
        DataType type = src;
        TreeNode<BaseDataType>* root = type.list.get_root();
        if (!root) return type;
        if (!root->data.is_object) {
            if (!root->data.is_simple() ||
                root->data.getsimpletype() != SimpleDataType::proc_ptr) {
                root->right = nullptr;
            }
            return type;
        }
        std::optional<Struct> st = struct_lookup(root->data.getobjectname());
        if (!st.has_value() || !st.value().temp) {
            root->right = nullptr;
            return type;
        }
        return type;
    }

    bool same_param_types(const std::vector<std::pair<std::string, DataType>>& a,
                          const std::vector<std::pair<std::string, DataType>>& b) {
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

    DataType type_of_dot(const NodeBinExprDot* dot, const Token& def) {
        DataType otype = type_of_expr(dot->lhs);
        if (std::holds_alternative<NodeTerm*>(dot->rhs->var)) {
            NodeTerm* id = std::get<NodeTerm*>(dot->rhs->var);
            if (!std::holds_alternative<NodeTermIdent*>(id->var)) {
                return BaseDataTypeVoid;
            }
            NodeTermIdent* tid = std::get<NodeTermIdent*>(id->var);
            Token ident = tid->ident;
            std::string field_name = ident.value.value();
            if (!otype.root().is_object) {
                return BaseDataTypeVoid;
            }
            std::string struct_name = otype.root().getobjectname();
            std::optional<Struct> st = struct_lookup(struct_name);
            if (st.has_value()) {
                std::optional<Field> field = field_lookup(st.value(), field_name);
                if (!field.has_value()) {
                    GeneratorError(def, "struct `" + struct_name + "` don't have field `" + field_name + "`");
                }
                Struct stc = st.value();
                Field  fd  = field.value();
                if (stc.temp) {
                    __stdvec<DataType> targs;
                    TreeNode<BaseDataType>* current = otype.list.get_root()->right;
                    for (size_t i = 0; i < stc.temps.size(); ++i) {
                        if (current == nullptr) {
                            GeneratorError(def, "Internal Compiler Error: malformed template type inside type_of_dot.");
                        }
                        size_t arg_len = count_template_nodes(current);
                        targs.push_back(extract_type_nodes(current, arg_len));
                        for (size_t k = 0; k < arg_len; ++k) {
                            if (current) current = current->right;
                        }
                    }
                    __map<std::string, DataType> temps = compute_temps(stc.temps, targs);
                    substitute_template_wct(fd.type, temps);
                }
                return fd.type;
            }
            std::optional<Interface> inter = inter_lookup(struct_name);
            if (inter.has_value()) {
                std::optional<Field> field = field_lookup(inter.value(), field_name);
                return field.value().type;
            }
            return BaseDataTypeVoid;
        } else {
            return BaseDataTypeVoid;
        }
    }

    DataType type_of_expr(const NodeExpr* expr) {
        DataType res = __type_of_expr(expr);
        substitute_template(res);
        res = canonical_type(res);
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
                if (!tpof.root().is_object || tpof.root().link)
                    GeneratorError(term_call->def, "can't call method from type " + tpof.to_string() + ".");
                NodeTermNmCall nmcall;
                nmcall.def = term_call->def;
                nmcall.nm = tpof.root().getobjectname();
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
                if (!tp.root().link && tp.root().ptrlvl == 0 && !tp.is_object() && tp.root() != SimpleDataType::ptr) {
                    GeneratorError(unref->def, "can't dereference not-reference or pointer type.");
                }
                if (tp.root().link) {
                    tp.root().link = false;
                    return tp;
                }
                if (tp.root().ptrlvl != 0ULL) {
                    tp.root().ptrlvl--;
                    return tp;
                }
                if (tp.is_object()) {
                    NodeTermMtCall deref;
                    deref.def = unref->def;
                    deref.mt = unref->expr;
                    deref.name = "m_deref";
                    std::vector<NodeExpr*> args;
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
                tp.root().ptrlvl += 1;
                return tp;
            }
            if (std::holds_alternative<NodeTermDrvalue*>(term->var)) {
                DataType tp = type_of_expr(std::get<NodeTermDrvalue*>(term->var)->expr);
                if (!tp.root().link) {
                    GeneratorError(std::get<NodeTermDrvalue*>(term->var)->def, "__disable_rvalue__ on a non-rvalue expression");
                }
                tp.root().link = false;
                return tp;
            }
            if (std::holds_alternative<NodeTermCall*>(term->var)) {
                NodeTermCall* call = std::get<NodeTermCall*>(term->var);
                std::string name = call->name;
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
                    __map<std::string, DataType> temps;
                    bool substituted = false;
                    if (call->targs.empty() && proc.templates == NULL) return proc.rettype;
                    else if (call->targs.empty() && call->args.has_value() && proc.templates != NULL) {
                        temps = try_derive_templates(call->targs, proc.params, call->def, proc.templates, __getargs(call->args.value()), proc);
                        substituted = true;
                    }
                    size_t counter{ 0 };
                    if (!substituted) {
                        if (proc.templates != NULL && call->targs.size() != proc.templates->size()) {
                            GeneratorError(call->def, "procedure `" + call->name + "` expects " + std::to_string(proc.templates->size()) + " template arguments, but got " + std::to_string(call->targs.size()));
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
                        GeneratorError(call->def, "struct `" + st.name + "` except " + std::to_string(st.temps.size()) + " template arguments in <...>, bug got " + std::to_string(call->targs.size()) + ".");
                    TreeNode<BaseDataType>* current = dt.list.get_root();
                    for (int i = 0; i < static_cast<int>(call->targs.size()); ++i) {
                        DataType arg_tp = call->targs[i];
                        substitute_template(arg_tp);
                        append_type_chain(dt, current, arg_tp);
                        while (current->right != nullptr) {
                            current = current->right;
                        }
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
                __map<std::string, DataType> temps;
                bool substituted = false;
                if (call->targs.empty() && proc.templates == NULL) return proc.rettype;
                else if (call->targs.empty() && call->args.has_value() && proc.templates != NULL) {
                    temps = try_derive_templates(call->targs, proc.params, call->def, proc.templates, __getargs(call->args.value()), proc);
                    substituted = true;
                }
                size_t counter{ 0 };
                if (!substituted) {
                    if (proc.templates != NULL && call->targs.size() != proc.templates->size()) {
                        GeneratorError(call->def, "procedure `" + call->name + "` expects " + std::to_string(proc.templates->size()) + " template arguments, but got " + std::to_string(call->targs.size()));
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
                    DataType tp = BaseDataTypeProcPtr;
                    tp.list.insert_right(prc.value().rettype.root(), tp.list.get_root());
                    return tp;
                }
                GeneratorError(std::get<NodeTermIdent*>(term->var)->ident, "unkown word `" + std::get<NodeTermIdent*>(term->var)->ident.value.value() + "`");
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
        }
        assert(false);
    }

    using mapped_temps = __map<std::string, DataType>;

    mapped_temps compute_temps(const __stdvec<std::string>& templates, const __stdvec<DataType>& targs) {
        assert(templates.size() == targs.size());
        mapped_temps temps;
        size_t counter{ 0 };
        for (auto&& el : templates) {
            temps[el] = targs[counter++];
        }
        return temps;
    }

    __map<std::string, Field> compute_fields(const std::vector<std::pair<std::string, DataType>>& fields) {
        __map<std::string, Field> __fields;
        size_t nth = 0ULL;
        for (const std::pair<std::string, DataType>& field : fields) {
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
        push_sym("s_" + std::to_string(idx));
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
                int32_t v = std::stol(term_int_lit->int_lit.value.value());
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
                if (tp.is_object() && !tp.root().link && tp.root().ptrlvl == 0) {
                    if (lvalue) {
                        NodeTermMtCall deref;
                        deref.def = term_unref->def;
                        deref.mt = term_unref->expr;
                        deref.name = "m_assign_deref";
                        std::vector<NodeExpr*> args;
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
                        std::vector<NodeExpr*> args;
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
                std::string name = term_ident->ident.value.value();
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
                std::vector<NodeExpr*> raw_args;
                if (term_call->args.has_value()) raw_args = gen.__getargs(term_call->args.value());
                std::string tsign = gen.instantiate_if_needed(proc, term_call->targs, raw_args, term_call->def, pname, nname);
                if (proc.rettype.root() == BaseDataTypeVoid)
                    gen.GeneratorError(term_call->def, "can't use void " + nname + "::" + pname + "(...) as value");
                size_t stack_allign = 0;
                if (!raw_args.empty() || proc.params.empty()) {
                    if (!raw_args.empty()) gen.__typecheck_call(raw_args, proc.params, term_call->def, proc, &stack_allign);
                } else {
                    gen.GeneratorError(term_call->def, "procedure `" + proc.name + "` expects " + std::to_string(proc.params.size()) + " args, but got 0");
                }
                gen.gen_args(raw_args, proc.params);
                std::string label = nname + "@" + pname + tsign;
                if (proc.override) label += proc.get_sign();
                gen.m_builder.call(gen.sym(label));
                if (stack_allign != 0) {
                    gen.m_builder.add(gen.reg(Reg::ESP), gen.imm(static_cast<int32_t>(stack_allign * 4)));
                }
                gen.push_reg(Reg::EAX);
            }

            void operator()(NodeTermCall* term_call) const {
                const std::string name = term_call->def.value.value();
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
                    std::vector<NodeExpr*> raw_args;
                    if (term_call->args.has_value()) raw_args = gen.__getargs(term_call->args.value());
                    std::string tsign = gen.instantiate_if_needed(proc, term_call->targs, raw_args, term_call->def, name, "");
                    size_t stack_allign = 0;
                    if (!raw_args.empty() || proc.params.empty()) {
                        if (!raw_args.empty()) gen.__typecheck_call(raw_args, proc.params, term_call->def, proc, &stack_allign);
                    } else {
                        gen.GeneratorError(term_call->def, "procedure `" + proc.name + "` expects " + std::to_string(proc.params.size()) + " args, but got 0");
                    }
                    gen.gen_args(raw_args, proc.params);
                    std::string label = name + tsign;
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
                        gen.GeneratorError(term_call->def, "struct `" + st.value().name + "` except " + std::to_string(st.value().temps.size()) + " template arguments in <...>, bug got " + std::to_string(term_call->targs.size()) + ".");
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
                    std::vector<NodeExpr*> iargs;
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
                            gen.GeneratorError(term_call->def, "except " + std::to_string(st.value().fields.size()) + " args\nNOTE: but got " + std::to_string(iargs.size()) + "\nNOTE: if you don't want initialize all fields dont provide any arguments");
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
                        __map<std::string, DataType> temps;
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
                                gen.GeneratorError(term_call->def, "missmatch in initializers types for field nth `" + std::to_string(i + 1) + "`\nNOTE: field name - `" + _st.__fields[i].first + "`" + "\nNOTE: excepted " + ftype.to_string() + "\nNOTE: but got " + itype.to_string());
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
                    NodeTermNmCall sb;
                    sb.def = base->def;
                    sb.nm = "std";
                    sb.name = "sub";
                    std::vector<NodeExpr*> args;
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
                if (oneT != twoT && !(oneT.root() == BaseDataTypePtr && twoT.root() == BaseDataTypeInt)) {
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
                    std::vector<NodeExpr*> args;
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
                    !(oneT.root().ptrlvl != 0ULL && twoT.root() == BaseDataTypeInt))
                    gen.GeneratorError(base->def, "can't use operator + for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                    std::vector<NodeExpr*> args;
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
                std::string flab = gen.create_label();
                std::string elab = gen.create_label();
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
                std::string flab = gen.create_label();
                std::string elab = gen.create_label();
                std::string tlab = gen.create_label();
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
                    std::vector<NodeExpr*> args;
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
                if (std::holds_alternative<NodeTerm*>(dot->rhs->var)) {
                    NodeTerm* id = std::get<NodeTerm*>(dot->rhs->var);
                    if (!std::holds_alternative<NodeTermIdent*>(id->var)) {
                        gen.GeneratorError(base->def, "after `.` except identificator");
                    }
                    NodeTermIdent* tid = std::get<NodeTermIdent*>(id->var);
                    Token ident = tid->ident;
                    std::string field_name = ident.value.value();
                    if (!otype.root().is_object) {
                        gen.GeneratorError(base->def, "bellow `.` except expression of type any object\nNOTE: but got " + otype.to_string());
                    }
                    std::string struct_name = otype.root().getobjectname();
                    std::optional<Struct> st = gen.struct_lookup(struct_name);
                    size_t field_offset = 0;
                    if (st.has_value()) {
                        std::optional<Field> field = gen.field_lookup(st.value(), field_name);
                        if (!field.has_value()) {
                            gen.GeneratorError(base->def, "object of type `" + otype.to_string() + "` doesn`t have field `" + field_name + "`");
                        }
                        field_offset = field.value().nth;
                    }
                    if (!st.has_value()) {
                        std::optional<Interface> inter = gen.inter_lookup(struct_name);
                        if (inter.has_value()) {
                            std::optional<Field> field = gen.field_lookup(inter.value(), field_name);
                            if (!field.has_value()) {
                                gen.GeneratorError(base->def, "object of type `" + otype.to_string() + "` doesn`t have field `" + field_name + "`");
                            }
                            field_offset = field.value().nth;
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
                } else {
                    gen.GeneratorError(base->def, "after `.` except identificator");
                }
            }

            void operator()(const NodeBinExprArgs* args) const {
                for (int i = static_cast<int>(args->args.size()) - 1; i > -1; --i) {
                    gen.gen_expr(args->args[i]);
                }
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
            } else if (std::holds_alternative<NodeStmtLetNoAssign*>(stmt->var)) {
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
            std::string objName = to.root().getobjectname();
            std::optional<Namespace*> _nms = namespace_lookup(objName);
            if (!_nms.has_value()) goto CONVERT_FAILED;
            NodeTermNmCall call;
            call.def = def;
            call.nm = objName;
            call.name = "new";
            std::vector<NodeExpr*> args;
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
                const std::string label = gen.create_label();
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

    void __typecheck_call(const std::vector<NodeExpr*>& args,
                          const std::vector<std::pair<std::string, DataType>>& params,
                          const Token& def,
                          const Procedure& proc,
                          size_t* stack_allign)
    {
        bool nosizedargs =
            std::find(proc.attrs.begin(), proc.attrs.end(), ProcAttr::nosizedargs) != proc.attrs.end();
        int bad = -1;
        if (!match_call_signature(args, params, proc, nosizedargs, &bad)) {
            GeneratorError(def,
                "procedure `" + proc.name + "`\nexcept type " +
                canonical_type(params[bad].second).to_string() + " at " + std::to_string(bad) +
                " argument\nNOTE: but found type " +
                canonical_type(type_of_expr(args[bad])).to_string());
        }

        *stack_allign += args.size();
    }

    void substitute_template(DataType& type) {
        if (m_temps.empty()) return;
        if (!type.root().is_object) return;

        std::string oname = type.root().getobjectname();
        const auto& env = m_temps.back();
        const auto it = env.find(oname);
        if (it != env.end()) {
            BaseDataType old = type.root();
            DataType mapped = it->second;

            TreeNode<BaseDataType>* root_node = type.list.get_root();
            bool has_template_args =
                (root_node->right != nullptr);
            bool mapped_has_children =
                (mapped.list.get_root() &&
                 mapped.list.get_root()->right != nullptr);

            if (has_template_args && !mapped_has_children) {
                BaseDataType new_root = mapped.root();
                new_root.ptrlvl = old.ptrlvl;
                new_root.link   = old.link;
                new_root.rvalue = old.rvalue;
                root_node->data = new_root;
            } else {
                type = mapped;
                type.root().ptrlvl += old.ptrlvl;
                if (old.link)   type.root().link   = true;
                if (old.rvalue) type.root().rvalue = true;
            }
        }

        TreeNode<BaseDataType>* cur = type.list.get_root()->right;
        while (cur != nullptr) {
            DataType tmp = cur->data;
            substitute_template(tmp);
            cur->data = tmp.root();
            TreeNode<BaseDataType>* extra = nullptr;
            TreeNode<BaseDataType>* tmp_root = tmp.list.get_root();
            if (tmp_root) extra = tmp_root->right;
            TreeNode<BaseDataType>* last = cur;
            while (extra != nullptr) {
                auto* new_node = new TreeNode<BaseDataType>(extra->data);
                new_node->right = last->right;
                last->right = new_node;
                last = new_node;
                extra = extra->right;
            }
            cur = last->right;
        }
    }

    void substitute_template_wct(DataType& type, __map<std::string, DataType>& temps) {
        if (!type.root().is_object) return;
        std::string oname = type.root().getobjectname();
        const auto it = temps.find(oname);
        if (it != temps.end()) {
            BaseDataType old = type.root();
            DataType mapped = it->second;
            TreeNode<BaseDataType>* root_node = type.list.get_root();
            bool has_template_args =
                (root_node->right != nullptr);
            bool mapped_has_children =
                (mapped.list.get_root() &&
                 mapped.list.get_root()->right != nullptr);
            if (has_template_args && !mapped_has_children) {
                BaseDataType new_root = mapped.root();
                new_root.ptrlvl = old.ptrlvl;
                new_root.link   = old.link;
                new_root.rvalue = old.rvalue;
                root_node->data = new_root;
            } else {
                type = mapped;
                type.root().ptrlvl += old.ptrlvl;
                if (old.link)   type.root().link   = true;
                if (old.rvalue) type.root().rvalue = true;
            }
        }
        TreeNode<BaseDataType>* cur = type.list.get_root()->right;
        while (cur != nullptr) {
            DataType tmp = cur->data;
            substitute_template_wct(tmp, temps);
            cur->data = tmp.root();
            TreeNode<BaseDataType>* extra = nullptr;
            TreeNode<BaseDataType>* tmp_root = tmp.list.get_root();
            if (tmp_root) extra = tmp_root->right;
            TreeNode<BaseDataType>* last = cur;
            while (extra != nullptr) {
                auto* new_node = new TreeNode<BaseDataType>(extra->data);
                new_node->right = last->right;
                last->right = new_node;
                last = new_node;
                extra = extra->right;
            }
            cur = last->right;
        }
    }

    bool __try_typecheck_call(const std::vector<NodeExpr*>& args, const Procedure& proc) {
        bool nosizedargs =
            std::find(proc.attrs.begin(), proc.attrs.end(), ProcAttr::nosizedargs) != proc.attrs.end();
        return match_call_signature(args, proc.params, proc, nosizedargs, nullptr);
    }

    std::vector<NodeExpr*> __getargs(NodeExpr* __expr) {
        return std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(__expr->var)->var)->args;
    }

    std::optional<int> __eval_ctcall(const NodeTermCall* call, const Token& def) {
        __str_ref name = call->name;
        if (name == "is_same_t") {
            if (!call->args.has_value()) {
                GeneratorError(def, "is_same_t excepts 2 args");
            }
            std::vector<NodeExpr*> args = __getargs(call->args.value());
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
            std::vector<NodeExpr*> args = __getargs(call->args.value());
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
            std::vector<NodeExpr*> args = __getargs(call->args.value());
            if (args.size() != 1) {
                GeneratorError(def, "ct_not excepts 1 args");
            }
            return static_cast<int>(!(static_cast<bool>(eval(args[0], def))));
        }
        return std::nullopt;
    }

    int eval(const NodeExpr* expr, const Token& def) {
        int result = 0;
        if (std::holds_alternative<NodeTerm*>(expr->var)) {
            NodeTerm* nterm = std::get<NodeTerm*>(expr->var);
            if (std::holds_alternative<NodeTermIntLit*>(nterm->var)) {
                return std::stoul(std::get<NodeTermIntLit*>(nterm->var)->int_lit.value.value());
            }
            if (std::holds_alternative<NodeTermCtMdefined*>(nterm->var)) {
                return static_cast<int>(std::get<NodeTermCtMdefined*>(nterm->var)->value);
            }
            if (std::holds_alternative<NodeTermIdent*>(nterm->var)) {
                std::string cname = std::get<NodeTermIdent*>(nterm->var)->ident.value.value();
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
        return result;
    }

    bool in_namespace() {
        return m_cur_namespace != NULL;
    }

    __map<std::string, DataType> try_derive_templates(std::vector<DataType>& targs,
                                                      const std::vector<std::pair<std::string, DataType>>& params,
                                                      const Token& def,
                                                      __stdvec<std::string>* templates,
                                                      const std::vector<NodeExpr*> args,
                                                      const Procedure& proc)
    {
        if (params.empty()) {
            GeneratorError(def, "can't substitute type " + *templates->begin() + ", params empty.");
        }
        if (params.size() != args.size()) {
            GeneratorError(def, "procedure `" + proc.name +
                                 "` excepts " + std::to_string(params.size()) +
                                 " args, but got " + std::to_string(args.size()) + ".");
        }

        assert(templates != NULL);
        assert(!templates->empty());

        __map<std::string, DataType> temps;
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

    std::pair<__map<std::string, DataType>, bool> try_derive_templates_no_err(
        std::vector<DataType> targs,
        const std::vector<std::pair<std::string, DataType>> params,
        const Token& def,
        __stdvec<std::string>* templates,
        const std::vector<NodeExpr*> args,
        const Procedure& proc)
    {
        using __L_map = __map<std::string, DataType>;

        if (params.empty()) {
            return std::make_pair(__L_map{}, false);
        }
        if (params.size() != args.size()) {
            GeneratorError(def, "procedure `" + proc.name +
                                 "` excepts " + std::to_string(params.size()) +
                                 " args, but got " + std::to_string(args.size()) + ".");
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
                    GeneratorError(def, "procedure `" + proc->name + "` excepts " + std::to_string(proc->params.size()) + " arguments, but got 0.");
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
                std::vector<NodeExpr*> args_v = __getargs(args.value());
                std::vector<DataType>  arg_types;
                for (auto* e : args_v) arg_types.push_back(canonical_type(type_of_expr(e)));
                std::string args_s = format_type_list(arg_types);
                DiagnosticMessage(def, "error", "no match candidate for call procedure " + proc->name + "(" + args_s + ").", 0);
                std::string args_s1;
                for (int i = 0; i < static_cast<int>(proc->params.size()); ++i) {
                    std::string tmp(proc->params[i].second.to_string());
                    args_s1 += tmp.substr(1, tmp.size() - 2);
                    if (i != static_cast<int>(proc->params.size()) - 1) {
                        args_s1 += ", ";
                    }
                }
                DiagnosticMessage(proc->def, "note", "candidate " + proc->name + "(" + args_s1 + ").", 0);
                for (int i = 0; i < static_cast<int>(proc->overrides.size()); ++i) {
                    Procedure* cp2 = proc->overrides[i];
                    std::string args_s2;
                    for (int j = 0; j < static_cast<int>(cp2->params.size()); ++j) {
                        std::string tmp(cp2->params[j].second.to_string());
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

    __stdvec<Procedure> collect_candidates_earg(Procedure* proc) {
        __stdvec<Procedure> res;
        if (proc->params.empty()) res.push_back(*proc);
        for (int i = 0; i < static_cast<int>(proc->overrides.size()); ++i) {
            if (proc->overrides[i]->params.empty()) res.push_back(*proc->overrides[i]);
        }
        return res;
    }

    __stdvec<Procedure> collect_candidates(Procedure* proc, size_t args_size) {
        __stdvec<Procedure> res;
        if (proc->params.size() == args_size) res.push_back(*proc);
        for (int i = 0; i < static_cast<int>(proc->overrides.size()); ++i) {
            if (proc->overrides[i]->params.size() == args_size) res.push_back(*proc->overrides[i]);
        }
        return res;
    }

    void resolve_overrides_tp(Procedure* proc,
                              std::optional<NodeExpr*> args,
                              const Token& def,
                              __stdvec<DataType> targs)
    {
        assert(proc != NULL);
        Procedure  copy_of_proc = *proc;
        Procedure* ptr_to_proc  = proc;
        __stdvec<Procedure> candidates;
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
            __map<std::string, DataType> temps;
            if (cur_c.templates != NULL && targs.empty()) {
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
            if (__try_typecheck_call(__getargs(args.value()), cur_c)) {
                *ptr_to_proc = candidates[i];
                return;
            }
        }
        std::vector<NodeExpr*> args_v = __getargs(args.value());
        std::vector<DataType>  arg_types;
        for (auto* e : args_v) arg_types.push_back(canonical_type(type_of_expr(e)));
        std::string args_s = format_type_list(arg_types);
        DiagnosticMessage(def, "error", "no match candidate for call procedure " + proc->name + "(" + args_s + ").", 0);
        std::string args_s1;
        for (int i = 0; i < static_cast<int>(proc->params.size()); ++i) {
            args_s1 += proc->params[i].second.to_string();
            if (i != static_cast<int>(proc->params.size()) - 1) {
                args_s1 += ", ";
            }
        }
        DiagnosticMessage(proc->def, "note", "candidate " + proc->name + "(" + args_s1 + ").", 0);
        for (int i = 0; i < static_cast<int>(proc->overrides.size()); ++i) {
            Procedure* cp = proc->overrides[i];
            std::string args_s2;
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

    void gen_args(const __stdvec<NodeExpr*>& args, __stdvec<std::pair<std::string, DataType>>& params) {
        for (int i = static_cast<int>(args.size()) - 1; i > -1; --i) {
            if (i < static_cast<int>(params.size())) {
                DataType tp = type_of_expr(args[i]);
                if (tp.root().link) gen_expr(args[i], false);
                else                gen_expr(args[i], params[i].second.root().link);
            } else gen_expr(args[i], false);
        }
    }

    void substitute_template_params(__map<std::string, DataType>& temps,
                                    __stdvec<std::pair<std::string, DataType>>& params) {
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
                gen.m_builder.call(gen.sym("ExitProcess@4"));
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
                            Procedure* ovs = gen.m_allocator.emplace<Procedure>();
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
                            std::to_string(proc.value().params.size()) + "` but got `" +
                            std::to_string(stmt_proc->params.size()) + "`.");
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
                        if (search != gen.m_cur_namespace->procs.end()) {
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

                            Procedure* ovs = gen.m_allocator.emplace<Procedure>();
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
                if (stmt_proc->prototype)                    return;
                if (override && movs->templates != NULL)     return;

                std::vector<ProcAttr> attrs = stmt_proc->attrs;
                bool noprolog = std::find(attrs.begin(), attrs.end(), ProcAttr::noprolog) != attrs.end();

                std::string label;
                if (stmt_proc->name != "main") {
                    if (gen.in_namespace()) {
                        label += gen.m_cur_namespace->name;
                        label += "@";
                    }
                    label += stmt_proc->name;
                    if (override) {
                        label += movs->get_sign();
                    }
                } else {
                    label = "__main";
                }

                if (gen.m_used_labels.find(label) != gen.m_used_labels.end()) {
                    gen.DiagnosticMessage(stmt_proc->def, "error",
                                          "procedure `" + stmt_proc->name + "` redefinition (ASM label `" +
                                          label + "` already used).", 0);
                    exit(1);
                }
                gen.m_used_labels.insert(label);

                gen.m_builder.label(label);

                if (!noprolog) {
                    gen.m_builder.push(gen.reg(Reg::EBP));
                    gen.m_builder.mov(gen.reg(Reg::EBP), gen.reg(Reg::ESP));
                }

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
                    gen.m_builder.call(gen.sym("traceback_pop"));
                } else {
                    gen.push_reg(Reg::EAX);
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
                                    std::string name = id->ident.value.value();
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
                gen.create_var(stmt_let->ident.value.value(), stmt_let->expr,
                               stmt_let->ident, stmt_let->type);
            }

            void operator()(const NodeStmtLetNoAssign* stmt_let) const
            {
                gen.create_var_va(stmt_let->ident.value.value(), stmt_let->type, stmt_let->ident);
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
                            std::string name = lvident->ident.value.value();
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
                    NodeStmtMtCall call;
                    call.def = stmt_assign->def;
                    call.mt  = stmt_assign->lvalue;
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
                const std::string name = stmt_call->def.value.value();
                Procedure proc = gen.__proc_get(stmt_call->name, stmt_call->def);
                if (!proc.overrides.empty())
                    gen.resolve_overrides_tp(&proc, stmt_call->args, stmt_call->def, stmt_call->targs);

                std::vector<NodeExpr*> raw_args;
                if (stmt_call->args.has_value()) raw_args = gen.__getargs(stmt_call->args.value());

                std::string tsign =
                    gen.instantiate_if_needed(proc, stmt_call->targs, raw_args,
                                              stmt_call->def, name, "");

                size_t stack_allign = 0;
                if (!raw_args.empty() || proc.params.empty()) {
                    if (!raw_args.empty())
                        gen.__typecheck_call(raw_args, proc.params, stmt_call->def,
                                             proc, &stack_allign);
                } else {
                    gen.GeneratorError(stmt_call->def, "procedure `" + proc.name + "` expects " +
                                                          std::to_string(proc.params.size()) +
                                                          " args, but got 0");
                }

                gen.gen_args(raw_args, proc.params);

                std::string label = name + tsign;
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
                const std::string label = gen.create_label();
                gen.m_builder.emit(IRInstr(IROp::Test, gen.reg(Reg::EAX), gen.reg(Reg::EAX)));
                gen.m_builder.jz(gen.label(label));
                gen.gen_scope(stmt_if->scope);
                if (stmt_if->pred.has_value()) {
                    const std::string end_label = gen.create_label();
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
                size_t current_type_id = (*gen.m_typeid_table_size)++;
                gen.m_typeid_table.push_back(std::make_pair(current_type_id, stmt_struct->name));
                gen.m_structs[stmt_struct->name] = {
                    .name       = stmt_struct->name,
                    .fields     = gen.compute_fields(stmt_struct->fields),
                    .__allocator = stmt_struct->__allocator,
                    .__fields   = stmt_struct->fields,
                    .m_typeid   = gen.m_structs_count++,
                    .temp       = stmt_struct->temp,
                    .temps      = stmt_struct->temps,
                    .def        = stmt_struct->def
                };
            }

            void operator()(const NodeStmtInterface* stmt_inter) const
            {
                gen.m_interfaces[stmt_inter->name] = {
                    .name    = stmt_inter->name,
                    .fields  = gen.compute_fields(stmt_inter->fields),
                    .__fields = stmt_inter->fields,
                    .m_typeid = gen.m_structs_count++
                };
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
                std::string objectName = type.getobjectname();
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
                        __stdvec<NodeExpr*> args;
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
                    std::string oname = tp.root().getobjectname();
                    std::optional<Namespace*> nm = gen.namespace_lookup(oname);
                    if (nm.has_value()) {
                        Namespace* nms = nm.value();
                        const auto& search = nms->procs.find("what");
                        if (search != nms->procs.end()) {
                            Procedure proc = search->second;
                            has_what = true;
                            std::string lbl = oname + "@what";
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
                std::optional<Namespace*> enm = gen.namespace_lookup(stmt_space->name);
                if (!enm.has_value()) {
                    Namespace* nm = gen.m_allocator.emplace<Namespace>();
                    nm->procs = {};
                    nm->name  = stmt_space->name;
                    gen.m_cur_namespace = nm;
                    gen.m_namespaces[nm->name] = nm;
                } else {
                    gen.m_cur_namespace = enm.value();
                }
                for (NodeStmt* stmt : stmt_space->scope->stmts) {
                    gen.gen_stmt(stmt);
                }
                gen.m_cur_namespace = NULL;
            }

            void operator()(NodeStmtImpl* stmt_impl) const
            {
                std::optional<Namespace*> enm = gen.namespace_lookup(stmt_impl->name);
                std::string iname = stmt_impl->name;
                std::optional<Struct> h_s = gen.struct_lookup(iname);
                if (!h_s.has_value())
                    gen.GeneratorError(stmt_impl->def, "unkown structure name `" + iname + "`.");
                if (enm.has_value()) {
                    gen.GeneratorError(stmt_impl->def, "redefenition of implementation struct `" + iname + "`.");
                }
                Namespace* nm = gen.m_allocator.emplace<Namespace>();
                nm->procs = {};
                nm->name  = stmt_impl->name;
                gen.m_cur_namespace = nm;
                gen.m_namespaces[nm->name] = nm;
                for (NodeStmt* stmt : stmt_impl->scope->stmts) {
                    if (!stmt_impl->temps.empty() && std::holds_alternative<NodeStmtProc*>(stmt->var)) {
                        NodeStmtProc* ps = std::get<NodeStmtProc*>(stmt->var);
                        std::string pname = ps->name;
                        if (std::find(stmt_impl->inst.begin(), stmt_impl->inst.end(), pname) ==
                            stmt_impl->inst.end()) {
                            if (ps->templates == NULL)
                                ps->templates = gen.m_parser->m_allocator.emplace<__stdvec<std::string>>();
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

                std::vector<NodeExpr*> raw_args;
                if (stmt_call->args.has_value()) raw_args = gen.__getargs(stmt_call->args.value());

                std::string tsign =
                    gen.instantiate_if_needed(proc, stmt_call->targs, raw_args,
                                              stmt_call->def, pname, nname);

                size_t stack_allign = 0;
                if (!raw_args.empty() || proc.params.empty()) {
                    if (!raw_args.empty())
                        gen.__typecheck_call(raw_args, proc.params, stmt_call->def, proc, &stack_allign);
                } else {
                    gen.GeneratorError(stmt_call->def,
                        "procedure `" + proc.name + "` expects " +
                        std::to_string(proc.params.size()) + " args, but got 0");
                }

                gen.gen_args(raw_args, proc.params);

                std::string label = nname + "@" + pname + tsign;
                if (proc.override) label += proc.get_sign();
                gen.m_builder.call(gen.sym(label));

                if (stack_allign != 0)
                    gen.m_builder.add(gen.reg(Reg::ESP),
                                      gen.imm(static_cast<int32_t>(stack_allign * 4)));
            }

            void operator()(const NodeStmtMtCall* stmt_call) const
            {
                DataType tpof = gen.type_of_expr(stmt_call->mt);
                if (!tpof.is_object() || tpof.root().link)
                    gen.GeneratorError(stmt_call->def, "can't call method from type " + tpof.to_string() + ".");
                NodeStmtNmCall nmcall;
                nmcall.def   = stmt_call->def;
                nmcall.nm    = tpof.getobjectname();
                nmcall.name  = stmt_call->name;
                nmcall.args  = stmt_call->args;
                nmcall.targs = stmt_call->targs;
                NodeStmt stmt;
                stmt.var = &nmcall;
                gen.gen_stmt(&stmt);
            }

            void operator()(const NodeStmtConst* stmt_const) const
            {
                gen.last_scope_cns()[stmt_const->name] = {
                    .name  = stmt_const->name,
                    .value = gen.eval(stmt_const->expr, stmt_const->def)
                };
            }

            void operator()(const NodeStmtTypedef* stmt_tdef) const
            {
                gen.last_scope_tdef()[stmt_tdef->name] = stmt_tdef->type;
            }

            void operator()(const NodeStmtTry* stmt_try) const
            {
                const std::string catch_lab = gen.create_label();
                const std::string end_catch = gen.create_label();
                const std::string end_lab   = gen.create_label();

                gen.m_builder.mov(
                    gen.reg(Reg::EAX),
                    gen.mem(MemRef::sym("traceback", 4096))
                );
                gen.push_reg(Reg::EAX);

                gen.m_builder.emit(
                    IRInstr(IROp::Inc, gen.mem(MemRef::sym("__exception_bufs_lvl")))
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
                gen.pop_reg(Reg::EAX);
                gen.m_builder.mov(
                    gen.mem(MemRef::sym("traceback", 4096)),
                    gen.reg(Reg::EAX)
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
                gen.last_scope().erase(gen.last_scope().find(stmt_try->name));
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
        };

        StmtVisitor visitor{ *this };
        std::visit(visitor, stmt->var);
    }

    [[nodiscard]] std::string gen_prog()
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

	    std::vector<PendingTemplateInstance> pending;
    	m_pending_templates = &pending;
	
	    m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("global __BpmDoubleExceptionTypeId")));
	    m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("global __BpmRecursionExceptionTypeId")));
	    m_builder.emit(IRInstr(IROp::InlineAsm, Operand::symbolOp("global __BpmSigSegvExceptionTypeId")));
	
	    for (const NodeStmt* stmt : m_prog->stmts) {
	        gen_stmt(stmt);
	    }

	    gen_all_template_instances();

	    m_builder.label("main");

	    m_builder.call(sym("__bpm_set_sigsegv_handler"));
	    m_builder.mov(mem(MemRef::sym("stack_base")), reg(Reg::EBP));

	    m_builder.mov(mem(MemRef::sym("__BpmDoubleExceptionTypeId")),   imm(0));
	    m_builder.mov(mem(MemRef::sym("__BpmRecursionExceptionTypeId")), imm(0));
	    m_builder.mov(mem(MemRef::sym("__BpmSigSegvExceptionTypeId")),  imm(0));

	    m_builder.push(imm(static_cast<int32_t>((*m_typeid_table_size) * 4ULL)));
	    m_builder.call(sym("malloc"));
	    m_builder.add(reg(Reg::ESP), imm(4));
	    m_builder.mov(mem(MemRef::sym("__type_id_table")), reg(Reg::EAX));

	    m_init_typeid_table_ir();

	    m_builder.call(sym("__main"));

	    m_builder.push(imm(0));
	    m_builder.call(sym("ExitProcess@4"));

	    bool has_oninits = !__oninits.empty();

	    m_builder.label("_BPM_init_");
	    if (has_oninits) {
	        m_builder.push(reg(Reg::EBP));
	        m_builder.mov(reg(Reg::EBP), reg(Reg::ESP));
	    }

	    m_builder.mov(mem(MemRef::sym("tmp_p")), imm(0));

	    auto init_builtin_id = [&](const std::string& struct_name, const std::string& global_name) {
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
    	        "s_" + std::to_string(p.second.index),
    	        p.second.value
    	    });
    	}
	
    	for (auto&& p : m_global_vars) {
    	    final_ir.globals.push_back({ p.second.name });
    	}
	    if (m_optimize) {
            iropt::optimize_ir(final_ir);
        }
    	std::stringstream result_ss;
    	AsmEmitter emitter(result_ss);
    	emitter.emit_program(final_ir);
    	return result_ss.str();
	}

    void get_props_from_parser(Parser* parser) noexcept {
        m_parser = parser;
        m_lines  = &(parser->m_lines);
    }

    bool m_optimize = false;

private:
    std::string format_type_list(const std::vector<DataType>& types) {
        std::string res;
        for (int i = 0; i < static_cast<int>(types.size()); ++i) {
            res += types[i].to_string();
            if (i + 1 < static_cast<int>(types.size())) res += ", ";
        }
        return res;
    }

    bool derive_templates_core(std::vector<DataType>& targs,
                               const std::vector<std::pair<std::string, DataType>>& params,
                               __stdvec<std::string>* templates,
                               const std::vector<NodeExpr*>& args,
                               UNUSED_ARG const Procedure& proc,
                               __map<std::string, DataType>& temps_out)
    {
        assert(templates != NULL);
        assert(!templates->empty());

        targs.clear();
        targs.reserve(templates->size());
        for (int i = 0; i < static_cast<int>(templates->size()); ++i) {
            targs.emplace_back(SimpleDataType::_void);
        }

        size_t redo_count = 0;
        __map<std::string, DataType> temps;

        while (true) {
            for (int i = 0; i < static_cast<int>(params.size()); ++i) {
                int  counter   = -1;
                int  temp_s    = -1;
                bool is_temp_s = false;

                for (int j = 0; j < static_cast<int>(templates->size()); ++j) {
                    if (!params[i].second.is_object()) continue;

                    if (templates->operator[](j) == params[i].second.getobjectname()) {
                        counter = j;
                        break;
                    }

                    TreeNode<BaseDataType>* current = params[i].second.list.get_root()->right;
                    temp_s = 0;
                    while (current != nullptr) {
                        if (current->data.is_object &&
                            templates->operator[](j) == current->data.getobjectname())
                        {
                            if (targs[j].is_simple() &&
                                targs[j].getsimpletype() == SimpleDataType::_void)
                            {
                                counter   = j;
                                is_temp_s = true;
                                break;
                            }
                        }
                        temp_s++;
                        current = current->right;
                    }
                    if (is_temp_s) break;
                    else temp_s = -1;
                }

                if (counter != -1) {
                    if (targs[counter].is_simple() &&
                        targs[counter].getsimpletype() == SimpleDataType::_void)
                    {
                        if (is_temp_s) {
                            DataType ct = type_of_expr(args[i]);
                            TreeNode<BaseDataType>* cur = ct.list.get_root()->right;

                            for (int k = 0; k < temp_s && cur; ++k) {
                                cur = cur->right;
                            }

                            if (cur) {
                                targs[counter] = create_datatype_from_chain(cur);
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

    bool match_call_signature(const std::vector<NodeExpr*>& args,
                              const std::vector<std::pair<std::string, DataType>>& params,
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

            if (!argtype.root().arg_eq(ex_type.root())) {
                if (bad_index) *bad_index = i;
                return false;
            }

            if (argtype.is_object() && ex_type.is_object()) {
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

	std::string instantiate_if_needed(
	    Procedure& proc,
	    std::vector<DataType>& local_targs,
	    const std::vector<NodeExpr*>& call_args,
	    const Token& def,
	    const std::string& name,
	    const std::string& nname /* = "" */
	) {
	    bool   is_method_of_struct  = false;
	    Struct ownerStruct;
	    size_t structTemplateCount  = 0;

	    if (!nname.empty()) {
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

	    __map<std::string, DataType> temps;
	    std::string                  tsign;

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

	        std::string oname = selfType.getobjectname();
	        if (oname != nname) {
	            GeneratorError(def, "method `" + proc.name + "` called with self of different type: expected `" +
	                                 nname + "`, got `" + oname + "`");
	        }

	        std::vector<DataType> structArgs = get_template_args(selfType);
	        if (structArgs.size() != structTemplateCount) {
	            GeneratorError(def, "internal compiler error: mismatch struct template args count for `" +
	                                 nname + "`");
	        }

	        for (size_t i = 0; i < structTemplateCount; ++i) {
	            temps[ownerStruct.temps[i]] = structArgs[i];
	        }

	        substitute_template_params(temps, proc.params);
	        substitute_template_wct(proc.rettype, temps);

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
	            return ""; 

	        if (proc.templates != NULL && local_targs.size() != proc.templates->size()) {
	            GeneratorError(def, "template args mismatch");
	        }

	        size_t counter = 0;
	        for (auto&& el : *proc.templates) {
	            DataType arg_dt = local_targs[counter++];
	            substitute_template(arg_dt);
	            temps[el] = arg_dt;
	        }

	        substitute_template_params(temps, proc.params);
	        substitute_template_wct(proc.rettype, temps);

	        for (int i = 0; i < static_cast<int>(local_targs.size()); ++i) {
	            DataType t = local_targs[i];
	            substitute_template(t);
	            tsign += t.sign();
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

	    return tsign;
	}

	struct PendingTemplateInstance {
    	Procedure                    proc;  
    	std::string                  tsign; 
    	std::string                  nname; 
    	__map<std::string, DataType> temps; 
	};

	std::vector<PendingTemplateInstance>* m_pending_templates = nullptr;

	void gen_template_instance(const PendingTemplateInstance& pt) {
	    IRBuilder old_builder = m_builder;
	    m_builder = IRBuilder(m_templ_ir);

	    m_temps.push_back(pt.temps);

	    Procedure proc          = pt.proc;
	    const std::string& tsign = pt.tsign;
	    const std::string& nname = pt.nname;

	    std::string lbl;
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

	    m_tsigns.push_back(tsign);
	    if (!nname.empty()) proc.mbn = nname;

	    m_cur_proc = proc;
	    gen_scope_sp(proc.scope, proc.from, proc);

	    push_reg(Reg::EAX);
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
                sym("s_" + std::to_string(_index))
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
        std::string lbl;
        if (in_namespace()) lbl += "__" + m_cur_namespace->name + "@";
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

    inline std::string create_label() noexcept
    {
        std::stringstream ss;
        ss << "L" << (*m_label_count)++;
        return ss.str();
    }

    const NodeProg* m_prog = nullptr;

	IRProgram* m_main_ir  = nullptr;
	IRProgram* m_templ_ir = nullptr;
	
	IRBuilder m_builder{ nullptr };

    __map<std::string, __stdvec<std::string>>* m_lines = nullptr;

    __stdvec<__map<std::string, Var>> m_vars;
    __map<std::string, String>        m_strings;
    __map<std::string, Procedure>     m_procs;
    __map<std::string, Struct>        m_structs;
    __map<std::string, GVar>          m_global_vars;
    __map<std::string, Interface>     m_interfaces;
    __map<std::string, Namespace*>    m_namespaces;

    Namespace* m_cur_namespace = nullptr;
    ArenaAllocator m_allocator;
    Parser*        m_parser = nullptr;

    size_t m_structs_count = 5;
    std::optional<Procedure> m_cur_proc;

    std::vector<std::string> m_breaks;
    VectorSim<size_t>        m_scopes;
    VectorSim<size_t>        m_scopes_vi;
    VectorSim<size_t>        m_break_scopes;

    __stdset<std::string> m_used_labels;

    __stdvec<__map<std::string, Constant>> m_consts;
    __stdvec<__map<std::string, DataType>> m_typedefs;

    __stdvec<std::pair<size_t, std::string>> m_typeid_table{
        {TYPEID_INT,  "int"},
        {TYPEID_PTR,  "ptr"},
        {TYPEID_VOID, "void"},
        {TYPEID_ANY,  "any"},
        {TYPEID_CHAR, "char"},
    };

    std::vector<std::string> m_cexterns{
        "ExitProcess@4",
        "malloc",
        "free",
        "memcpy",
        "memalloc",
        "memfree",
        "heap_collect",
        "stack_base",
        "dump_all_chunks",
        "__current_exception",
        "__bpm_exception_throwed",
        "__bpm_start_catch",
        "__bpm_terminate",
        "__bpm_allocate_exception",
        "__bpm_throw",
        "__type_id_table",
        "_setjmp",
        "longjmp",
        "__bpm_setjmp_cur",
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
    };

    __stdvec<NodeScope*>                __oninits;
    __stdvec<std::string>               m_tsigns;
    __stdvec<__map<std::string, DataType>> m_temps;

    size_t* m_string_index      = nullptr;
    size_t  CTX_IOTA            = 0ULL;
    size_t  m_var_index         = 0ULL;
    size_t* m_label_count       = nullptr;
    size_t* m_typeid_table_size = nullptr;
};