#pragma once

#include "parser.hpp"

#define VectorSimDataCap 4096

#define TYPEID_INT  0
#define TYPEID_PTR  1
#define TYPEID_VOID 2
#define TYPEID_ANY  3
#define TYPEID_CHAR  4

#define __DTOR_PREFIX "__dtor__"

#define v_alt(__v, __tp) std::holds_alternative<__tp>(__v)

#define v_get(__v, __tp) std::get<__tp>(__v)

#define UNUSED_ARG __attribute__((unused))

using __str_ref = const std::string&;

void consume_un(...) {}

template<typename T>
class VectorSim {
private:
	T m_data[VectorSimDataCap];
	size_t m_size = 0ULL;
public:
	inline size_t size() const noexcept {
		return m_size;
	}
	inline void pop_back() noexcept {
		m_size--;
	}
	inline T& operator[](const size_t _A_index) noexcept {
		return m_data[_A_index];
	}
	inline void push_back(const T& _A_element) noexcept {
		m_data[m_size++] = _A_element;
	}
};

class Generator {
public:

	size_t typeid_of(const DataType& type) noexcept {
		if(!type.root().is_object) {
			SimpleDataType stype = type.root().getsimpletype();
			switch(stype) {
			case SimpleDataType::_int:
				return TYPEID_INT;
			case SimpleDataType::_void:
				return TYPEID_VOID;
			case SimpleDataType::ptr:
				return TYPEID_PTR;
			case SimpleDataType::any:
				return TYPEID_ANY;
			case SimpleDataType::_char:
				return TYPEID_CHAR;
			case SimpleDataType::_constexpr:
				return 0ULL;
			case SimpleDataType::proc_ptr:
				return 0ULL;
			}
		}
		else {
			std::string sname = type.root().getobjectname();
			std::optional<Struct> st = struct_lookup(sname);
			if(st.has_value()) {
				return st.value().m_typeid;
			}
			std::optional<Interface> st2 = inter_lookup(sname);
			if(st2.has_value()) {
				return st2.value().m_typeid;
			}
		}
		assert(false);
	}

	size_t sizeof_of(DataType& type) noexcept {
		if(type.root().is_object) {
			std::string name = type.root().getobjectname();
			std::optional<Struct> st = struct_lookup(name);
			if(st.has_value()) {
				return st.value().fields.size() * 4ULL;
			}
			std::optional<Interface> inter = inter_lookup(name);
			if(inter.has_value()) {
				return inter.value().fields.size() * 4ULL;
			}
			return 0ULL;
		}
		else {
			return 4ULL;
		}
	}

	struct AsmGen {
		Generator* gen;
		void add(__str_ref one, __str_ref two) const {
			gen->m_output << "    add " << one << ", " << two << "\n";
		}
		void sub(__str_ref one, __str_ref two) const {
			gen->m_output << "    sub " << one << ", " << two << "\n";
		}
		void mul(__str_ref one, __str_ref two) const {
			bool is_eax = one == "eax";
			if(!is_eax) {
				gen->m_output << "    mov eax, " << one << "\n";
			}
			gen->m_output << "    imul " << two << "\n";
			if(!is_eax) {
				gen->m_output << "    mov " << one << ", eax\n";
			}
		}
		void pop(__str_ref to) const {
			gen->m_output << "    pop " << to << "\n";
		}
		void push(__str_ref to) const {
			gen->m_output << "    push " << to << "\n";
		}
	};

	struct Var {
		std::string name {};
		size_t stack_loc {};
		DataType type;

		std::string ref() {
		return "dword [ebp-" + std::to_string(stack_loc) + "]";
	}
	};
	struct GVar {
		std::string name;
		DataType type;
	};
	struct Procedure {
		std::string name {};
		std::vector<std::pair<std::string, DataType>> params {};
		DataType rettype;
		size_t stack_allign;
		std::vector<ProcAttr> attrs;
		Token def;
		bool prototype;
		std::vector<Procedure*> overrides;
		bool override;
		std::optional<int> uniq_sign;
		__stdvec<std::string>* templates;
		const NodeScope* scope;
		const NodeStmtProc* from;
		__map<std::string, bool> instanceated;
		std::string mbn;
		int overload_nth = 0;

		std::string get_sign() {
			if(params.size() == 0ULL) {
				if(!uniq_sign.has_value()) uniq_sign = rand() % 1000;
				return std::to_string(uniq_sign.value());
			}
			std::string res;
			for(size_t i = 0; i < params.size(); ++i) {
				// Теперь просто вызываем sign(), он сам обработает вложенность
				res += params[i].second.sign();
			}
			return res;
		}

		void gen_ret(Generator& gen, std::optional<std::string> cnm) {
			size_t allign = gen.__compute_allign_ret();
			if(allign != 0) {
				gen.m_output << "    add esp, " << allign * 4 << "\n";
			}
			if(cnm.has_value()) gen.m_output << "    jmp __" << cnm.value() << "@";
			else gen.m_output << "    jmp __";
			if(!gen.m_tsigns.empty() && !mbn.empty() && !cnm.has_value()) gen.m_output << mbn << "@";
			gen.m_output << name;
			if(!gen.m_tsigns.empty()) gen.m_output << gen.m_tsigns.back();
			if(override) gen.m_output << get_sign();
			gen.m_output << "@ret\n";
		}

		void call(Generator& gen, const size_t allign) {
			gen.m_output << "    call " << name << "\n";
			if(allign != 0ULL) {
				gen.m_output << "    add esp, " << allign << "\n";
			}
		}
	};
	struct String {
		std::string value {};
		size_t index {};
	};
	struct Struct {
		std::string name;
		__map<std::string, Field> fields;
		std::optional<std::string> __allocator;
		__stdvec<std::pair<std::string, DataType>> __fields;
		size_t m_typeid;
		bool temp;
		__stdvec<std::string> temps;

		size_t size_of() const noexcept {
			return __fields.size();
		}

		bool has_allocator() const noexcept {
			return __allocator.has_value();
		}

		void alloc(Generator& gen) const noexcept {
			gen.m_output << "    push " << size_of() * 4ULL << "\n";
			if(has_allocator()) gen.m_output << "    call " << __allocator.value() << "\n";
			else gen.m_output << "    call memalloc\n";
			gen.m_output << "    add esp, 4\n";
		}

		void dealloc(Generator& gen) {
			gen.m_output << "    call memfree\n";
			gen.m_output << "    add esp, 4\n";
		}

		void call_dtor(Generator& gen, const std::string& offset, const Token& def) const {
			if(has_allocator()) gen.GeneratorWarning(def, "deleting object with custom allocator function.");
			std::optional<Procedure> __dtor = gen.proc_lookup(__DTOR_PREFIX + name);
			if(!__dtor.has_value()) return;
			gen.push(offset);
			__dtor.value().call(gen, 4ULL);
		}

		void call_dtor_s(Generator& gen, const Token& def) const {
			if(has_allocator()) gen.GeneratorWarning(def, "deleting object with custom allocator function.");
			std::optional<Procedure> __dtor = gen.proc_lookup(__DTOR_PREFIX + name);
			if(!__dtor.has_value()) return;
			gen.m_output << "    push dword [esp]\n";
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
		Token def;
	};

	class Executor {
	public:
		class ReturnException {
		public:
			ReturnException(const int _value) : value(_value) {}
			int get_value() {
				return value;
			}
		private:
			int value;
		};
		Executor() = delete;
		Executor(Generator& _gen, Token& _where) : gen(_gen) {
			where = _where;
		}
		int execute_expr(const NodeExpr* expr) {
			if(std::holds_alternative<NodeTerm*>(expr->var)) {
				auto term = std::get<NodeTerm*>(expr->var);
				if(std::holds_alternative<NodeTermIntLit*>(term->var)) {
					return std::stoul(std::get<NodeTermIntLit*>(term->var)->int_lit.value.value());
				}
			}
			gen.GeneratorError(where, "procedure is not constant-evaluatable.");
			return 0;
		}
		void execute_stmt(const NodeStmt* stmt) {
			if(std::holds_alternative<NodeStmtReturn*>(stmt->var)) {
				auto ret = std::get<NodeStmtReturn*>(stmt->var);
				if(!ret->expr.has_value()) {
					gen.GeneratorError(ret->def, "return without value in compile-time execution.");
				}
				throw ReturnException(execute_expr(ret->expr.value()));
			} else {
				gen.GeneratorError(where, "procedure is not constant-evaluatable.");
			}
		}
		void execute(Procedure& proc) {
			try {
				for(NodeStmt* stmt : proc.from->scope->stmts) {
					execute_stmt(stmt);
				}
			} catch(...) {
				throw;
			}
		}
		std::optional<int> var_lookup(__str_ref name) noexcept {
			for(int i = static_cast<int>(m_vars.size()) - 1;i > -1;--i) {
				const auto& search = m_vars[i].find(name);
				if(search != m_vars[i].end()) {
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
		: m_prog(prog), m_allocator(4 * 1024 * 1024) // 4 mb
	{
	}

	Generator(const Generator& other)
	: m_allocator(12 * 1024)
	{
		m_typeid_table = other.m_typeid_table;
		m_consts = other.m_consts;
		m_typedefs = other.m_typedefs;
		m_string_index = other.m_string_index;
		m_prog = other.m_prog;
		m_lines = other.m_lines;
		m_strings = other.m_strings;
		m_procs = other.m_procs;
		m_structs = other.m_structs;
		m_global_vars = other.m_global_vars;
		m_interfaces = other.m_interfaces;
		m_namespaces = other.m_namespaces;
		m_cur_namespace = NULL;
		m_parser = other.m_parser;
		m_structs_count = other.m_structs_count;
		m_consts = other.m_consts;
		CTX_IOTA = other.CTX_IOTA;
		m_label_count = other.m_label_count;
		m_result = other.m_result;
	}

	/*procedure lookup on only last scope.
	creating var (keyword `let`) use it function*/
	std::optional<Var> var_lookup_cs(__str_ref name) noexcept {
		__map<std::string, Var>& vrs = last_scope();
		const auto& search = vrs.find(name);
		if(search != vrs.end()) {
			return search->second;
		}
		return std::nullopt;
	}

	/*it lookup all scopes and find in they
	var with name `name`*/
	std::optional<Var> var_lookup(__str_ref name) noexcept {
		// i = scope than contains vars of current nth scope
		for(int i = static_cast<int>(m_vars.size()) - 1;i > -1;--i) {
			const auto& search = m_vars[i].find(name);
			if(search != m_vars[i].end()) {
				return search->second;
			}
		}
		return std::nullopt;
	}

	std::optional<DataType> typedef_lookup(__str_ref name) noexcept {
		// i = scope than contains vars of current nth scope
		for(int i = static_cast<int>(m_typedefs.size()) - 1;i > -1;--i) {
			const auto& search = m_typedefs[i].find(name);
			if(search != m_typedefs[i].end()) {
				return search->second;
			}
		}
		return std::nullopt;
	}

	/*it function lookup ny name var
	and if it doesnt find, it throw a error*/
	Var var_lookup_err(__str_ref name, const Token& def) noexcept {
		const std::optional<Var> v = var_lookup(name);
		if(!v.has_value()) GeneratorError(def, "unkown variable `" + name + "`");
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
		for(int i = static_cast<int>(m_consts.size()) - 1;i > -1;--i) {
			const auto& search = m_consts[i].find(name);
			if(search != m_consts[i].end()) {
				return search->second;
			}
		}
		return std::nullopt;
	}

	std::optional<GVar> gvar_lookup(__str_ref name) noexcept {
		const auto& search = m_global_vars.find(name);
		if(search != m_global_vars.end()) {
			return search->second;
		}
		return std::nullopt;
	}

	std::optional<Interface> inter_lookup(__str_ref name) {
		const auto& search = m_interfaces.find(name);
		if(search != m_interfaces.end()) {
			return search->second;
		}
		return std::nullopt;
	}

	std::optional<Procedure> proc_lookup(__str_ref name) noexcept {
		const auto& search = m_procs.find(name);
		if(search != m_procs.end()) {
			return search->second;
		}
		return std::nullopt;
	}

	std::optional<Struct> struct_lookup(__str_ref name) noexcept {
		const auto& search = m_structs.find(name);
		if(search != m_structs.end()) {
			return search->second;
		}
		return std::nullopt;
	}

	std::optional<Namespace*> namespace_lookup(__str_ref name) noexcept {
		const auto& search = m_namespaces.find(name);
		if(search != m_namespaces.end()) {
			return search->second;
		}
		return std::nullopt;
	}

	/*it is like var_lookup, but in struct*/
	std::optional<Field> field_lookup(const Struct& st, __str_ref field) const noexcept {
		const auto& search = st.fields.find(field);
		if(search != st.fields.end()) {
			return search->second;
		}
		return std::nullopt;
	}

	std::optional<Field> field_lookup(const Interface& st, __str_ref field) const noexcept {
		const auto& search = st.fields.find(field);
		if(search != st.fields.end()) {
			return search->second;
		}
		return std::nullopt;
	}

	std::optional<String> string_lookup(__str_ref svalue) noexcept {
		const auto& search = m_strings.find(svalue);
		if(search != m_strings.end()) {
			return search->second;
		}
		return std::nullopt;
	}

	void DiagnosticMessage(const Token& tok, __str_ref header, __str_ref msg, const int col_inc) {
		m_parser->DiagnosticMessage(tok, header, msg, col_inc);
	}

	/*function throwing error with location*/
	void GeneratorError(const Token& tok, __str_ref msg) {
		DiagnosticMessage(tok, "error", msg, 0);
		exit(EXIT_FAILURE);
	}

	/*function throwing warning with location*/
	void GeneratorWarning(const Token& tok, __str_ref msg) {
		DiagnosticMessage(tok, "warning", msg, 0);
	}

	DataType extract_full_type(TreeNode<BaseDataType>* start_node) {
        DataType dt;
        if (!start_node) return dt;

        // Копируем корень
        dt = start_node->data;

        // Копируем хвост
        if (start_node->right) {
            TreeNode<BaseDataType>* src = start_node->right;
            TreeNode<BaseDataType>* dest = dt.list.get_root();
            
            while(src != nullptr) {
                dt.list.insert_right(src->data, dest);
                dest = dest->right;
                src = src->right;
            }
        }
        return dt;
    }

    DataType create_datatype_from_chain(TreeNode<BaseDataType>* start_node) {
        DataType dt;
        if (!start_node) return dt;

        // 1. Сначала обязательно создаем корень
        dt.list.insert_data(start_node->data, dt.list.get_root_ptr());
        
        // 2. Теперь копируем хвост
        TreeNode<BaseDataType>* src = start_node->right;
        TreeNode<BaseDataType>* dest = dt.list.get_root(); // Теперь это не nullptr

        while(src != nullptr) {
            dt.list.insert_right(src->data, dest);
            
            // Сдвигаем указатель назначения на только что созданный узел.
            // В BinaryTree insert_right создает узел в dest->right.
            dest = dest->right; 
            
            src = src->right;
        }
        return dt;
    }

	void append_type_chain(DataType& target_dt, TreeNode<BaseDataType>* target_node, DataType& source_dt) {
		TreeNode<BaseDataType>* src_curr = source_dt.list.get_root(); // Берем корень типа-аргумента (например AAA)
		TreeNode<BaseDataType>* dst_curr = target_node;
		
		while(src_curr != nullptr) {
			target_dt.list.insert_right(src_curr->data, dst_curr);
			dst_curr = dst_curr->right;
			src_curr = src_curr->right;
		}
	}

	size_t count_template_nodes(TreeNode<BaseDataType>* start_node) {
        if (!start_node) return 0;
        
        size_t total_nodes = 1; // Текущий узел (например, AAA)
        
        if (start_node->data.is_object) {
            std::string name = start_node->data.getobjectname();
            size_t expected_args = 0;
            
            std::optional<Struct> st = struct_lookup(name);
            if (st.has_value() && st.value().temp) {
                expected_args = st.value().temps.size();
            } 
            // Если есть интерфейсы с шаблонами, добавить проверку inter_lookup здесь

            // Указатель на первый аргумент (следующий в списке right)
            TreeNode<BaseDataType>* child_iter = start_node->right;
            
            for(size_t i = 0; i < expected_args; ++i) {
                if (!child_iter) break; // Защита от битых типов

                // Рекурсивно узнаем, сколько узлов занимает этот аргумент
                size_t arg_len = count_template_nodes(child_iter);
                total_nodes += arg_len;
                
                // Пропускаем все узлы этого аргумента, чтобы перейти к следующему
                for(size_t k = 0; k < arg_len; ++k) {
                    if (child_iter) child_iter = child_iter->right;
                }
            }
        }
        
        return total_nodes;
    }

    // Создает новый DataType, копируя count узлов из цепочки start_node
    DataType extract_type_nodes(TreeNode<BaseDataType>* start_node, size_t count) {
        DataType dt;
        if (count == 0 || !start_node) return dt;
        
        // Копируем корень
        dt = start_node->data; 
        
        TreeNode<BaseDataType>* src = start_node->right;
        TreeNode<BaseDataType>* dst = dt.list.get_root();
        
        // Копируем хвост (count-1 узлов)
        for(size_t i = 1; i < count; ++i) {
            if(!src) break;
            dt.list.insert_right(src->data, dst);
            
            // insert_right создает узел справа, сдвигаем dst на него
            dst = dst->right; 
            src = src->right;
        }
        return dt;
    }
	/*function returns type of field {}.{}*/
	DataType type_of_dot(const NodeBinExprDot* dot, const Token& def) {
        DataType otype = type_of_expr(dot->lhs);
        if(std::holds_alternative<NodeTerm*>(dot->rhs->var)) {
            NodeTerm* id = std::get<NodeTerm*>(dot->rhs->var);
            if(!std::holds_alternative<NodeTermIdent*>(id->var)) {
                return BaseDataTypeVoid;
            }
            NodeTermIdent* tid = std::get<NodeTermIdent*>(id->var);
            Token ident = tid->ident;
            std::string field_name = ident.value.value();
            if(!otype.root().is_object) {
                return BaseDataTypeVoid;
            }
            std::string struct_name = otype.root().getobjectname();
            std::optional<Struct> st = struct_lookup(struct_name);
            
            if(st.has_value()) {
                std::optional<Field> field = field_lookup(st.value(), field_name);      
                if(!field.has_value()) {
                    GeneratorError(def, "struct `" + struct_name + "` don't have field `" + field_name + "`");
                }
                Struct stc = st.value();
                Field fd = field.value();
                
                // --- ИСПРАВЛЕННАЯ ЛОГИКА ШАБЛОНОВ ---
                if(stc.temp) {
                    __stdvec<DataType> targs;
                    TreeNode<BaseDataType>* current = otype.list.get_root()->right;
                    
                    // Итерируемся ровно по количеству ожидаемых параметров шаблона
                    for(size_t i = 0; i < stc.temps.size(); ++i) {
                        if (current == nullptr) {
                            GeneratorError(def, "Internal Compiler Error: malformed template type inside type_of_dot.");
                        }

                        // 1. Считаем длину текущего типа-аргумента
                        size_t arg_len = count_template_nodes(current);
                        
                        // 2. Извлекаем его в отдельный DataType
                        targs.push_back(extract_type_nodes(current, arg_len));
                        
                        // 3. Сдвигаем current на следующий аргумент
                        for(size_t k = 0; k < arg_len; ++k) {
                            if (current) current = current->right;
                        }
                    }
                    
                    __map<std::string, DataType> temps = compute_temps(stc.temps, targs);
                    substitute_template_wct(fd.type, temps);
                }
                // -------------------------------------

                return fd.type;
            }
            
            std::optional<Interface> inter = inter_lookup(struct_name);
            if(inter.has_value()) {
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
		return res;
	}

	DataType __type_of_expr(const NodeExpr* expr) {
		if(holds_alternative<NodeTerm*>(expr->var)) {
			NodeTerm* term = std::get<NodeTerm*>(expr->var);
			if(std::holds_alternative<NodeTermIntLit*>(term->var)) {
				return BaseDataTypeInt;
			}
			if(std::holds_alternative<NodeTermCtEval*>(term->var)) {
				return type_of_expr(std::get<NodeTermCtEval*>(term->var)->expr);
			}
			if(std::holds_alternative<NodeTermLine*>(term->var)) {
				return BaseDataTypeInt;
			}
			if(std::holds_alternative<NodeTermCol*>(term->var)) {
				return BaseDataTypeInt;
			}
			if(std::holds_alternative<NodeTermCtMdefined*>(term->var)) {
				return BaseDataTypeInt;
			}
			if(std::holds_alternative<NodeTermFile*>(term->var)) {
				return BaseDataTypePtr;
			}
			if(std::holds_alternative<NodeTermStrLit*>(term->var)) {
				BaseDataType tp = BaseDataTypeChar;
				tp.ptrlvl += 1;
				return tp;
			}
			if(std::holds_alternative<NodeTermCast*>(term->var)) {
				return std::get<NodeTermCast*>(term->var)->type;
			}
			if(std::holds_alternative<NodeTermMtCall*>(term->var)) {
				NodeTermMtCall* term_call = std::get<NodeTermMtCall*>(term->var);
				DataType tpof = type_of_expr(term_call->mt);
				if(!tpof.root().is_object || tpof.root().link) GeneratorError(term_call->def, "can't call method from type " + tpof.to_string() + ".");
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
			if(std::holds_alternative<NodeTermRd*>(term->var)) {
				return BaseDataTypeInt;
			}
			if(std::holds_alternative<NodeTermParen*>(term->var)) {
				return type_of_expr(std::get<NodeTermParen*>(term->var)->expr);
			}
			if(std::holds_alternative<NodeTermUnref*>(term->var)) {
				NodeTermUnref* unref = std::get<NodeTermUnref*>(term->var);
				DataType tp = type_of_expr(unref->expr);
				if(!tp.root().link && tp.root().ptrlvl == 0 && !tp.is_object()) {
					GeneratorError(unref->def, "can't dereference not-reference or pointer type.");
				}
				if(tp.root().link) {
					tp.root().link = false;
					return tp;
				}
				if(tp.root().ptrlvl != 0ULL) {
					tp.root().ptrlvl--;
					return tp;
				}
				if(tp.is_object()) {
					NodeTermMtCall deref;
					deref.def = unref->def;
					deref.mt = unref->expr;
					deref.name = "m_deref";
					std::vector<NodeExpr*> args;
					args.push_back(unref->expr);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = unref->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					deref.args = &eargs;
					NodeTerm asterm { .var = &deref };
					NodeExpr asexpr { .var = &asterm };
					return type_of_expr(&asexpr);
				}
				return tp;
			}
			if(std::holds_alternative<NodeTermCastTo*>(term->var)) {
				return type_of_expr(std::get<NodeTermCastTo*>(term->var)->to);
			}
			if(std::holds_alternative<NodeTermSizeof*>(term->var)) {
				return BaseDataTypeInt;
			}
			if(std::holds_alternative<NodeTermExprStmt*>(term->var)) {
				return type_of_expr(std::get<NodeTermExprStmt*>(term->var)->expr);
			}
			if(std::holds_alternative<NodeTermTypeid*>(term->var)) {
				return BaseDataTypeInt;
			}
			if(std::holds_alternative<NodeTermPop*>(term->var)) {
				return BaseDataTypeAny;
			}
			if(std::holds_alternative<NodeTermAmpersand*>(term->var)) {
				DataType tp = type_of_expr(std::get<NodeTermAmpersand*>(term->var)->expr);
				tp.root().ptrlvl += 1;
				return tp;
			}
			if(std::holds_alternative<NodeTermDrvalue*>(term->var)) {
				DataType tp = type_of_expr(std::get<NodeTermDrvalue*>(term->var)->expr);
				if (!tp.root().link) {
					GeneratorError(std::get<NodeTermDrvalue*>(term->var)->def, "__disable_rvalue__ on a non-rvalue expression");
				}
				tp.root().link = false;
				return tp;
			}
			if(std::holds_alternative<NodeTermCall*>(term->var)) {
				NodeTermCall* call = std::get<NodeTermCall*>(term->var);
				std::string name = call->name;
				std::optional<Procedure> _proc = proc_lookup(name);
				if(_proc.has_value()) {
					Procedure proc = _proc.value();
					if(proc.rettype == BaseDataTypeConst) return BaseDataTypeInt;
					if(!proc.overrides.empty()) {
						resolve_overrides_tp(&proc, call->args, call->def, call->targs);
					}
					if(proc.templates == NULL || !_proc.value().rettype.root().is_object) {
						return proc.rettype;
					}
					__map<std::string, DataType> temps;
					bool substituted = false;
					if(call->targs.empty() && proc.templates == NULL) return proc.rettype;
					else if(call->targs.empty() && call->args.has_value() && proc.templates != NULL) {
						temps = try_derive_templates(call->targs, proc.params, call->def, proc.templates, __getargs(call->args.value()), proc);
						substituted = true;
					}
					size_t counter {0};
					if(!substituted) {
                        // --- FIX START ---
						if (proc.templates != NULL && call->targs.size() != proc.templates->size()) {
							GeneratorError(call->def, "procedure `" + call->name + "` expects " + std::to_string(proc.templates->size()) + " template arguments, but got " + std::to_string(call->targs.size()));
						}
                        // --- FIX END ---
						for(auto&& el : *proc.templates) {
							temps[el] = call->targs[counter++];
						}
					}
					const auto& search = temps.find(proc.rettype.root().to_string_d());
					if(search != temps.end()) proc.rettype = search->second;
					return proc.rettype;
				}
				std::optional<Struct> _st = struct_lookup(call->name);
				if(_st.has_value()) {
					Struct st = _st.value();
					BaseDataType bs = st.name;
					DataType dt = bs;
					if(st.temp && call->targs.size() != st.temps.size()) GeneratorError(call->def, "struct `" + st.name + "` except " + std::to_string(st.temps.size()) + " template arguments in <...>, bug got " + std::to_string(call->targs.size()) + ".");
					TreeNode<BaseDataType>* current = dt.list.get_root();
					
					// --- НАЧАЛО ИЗМЕНЕНИЙ ---
					for (int i = 0; i < static_cast<int>(call->targs.size()); ++i) {
    					DataType arg_tp = call->targs[i];    // копия
    					substitute_template(arg_tp);         // подставляем шаблоны только в копии
    					append_type_chain(dt, current, arg_tp);
    					while (current->right != nullptr) {
    					    current = current->right;
    					}
					}
					// --- КОНЕЦ ИЗМЕНЕНИЙ ---
					
					return dt;
				}
				GeneratorError(call->def, "unkown procedure `" + name + "`");
				return BaseDataTypeVoid;
			}
			if(std::holds_alternative<NodeTermNmCall*>(term->var)) {
				NodeTermNmCall* call = std::get<NodeTermNmCall*>(term->var);
				__str_ref name = call->name;
				__str_ref nmsp = call->nm;
				std::optional<Namespace*> nms = namespace_lookup(nmsp);
				if(!nms.has_value()) GeneratorError(call->def, "unkown namespace `" + nmsp + "`");
				const auto& search = nms.value()->procs.find(name);
				if(search == nms.value()->procs.end()) GeneratorError(call->def, "namespace `" + nmsp + "` doesn't have procedure `" + name + "`");
				Procedure proc = search->second;
				if(!proc.overrides.empty()) {
					resolve_overrides_tp(&proc, call->args, call->def, call->targs);
				}
				if(proc.templates == NULL || !proc.rettype.root().is_object) {
					return proc.rettype;
				}
				__map<std::string, DataType> temps;
				bool substituted = false;
				if(call->targs.empty() && proc.templates == NULL) return proc.rettype;
				else if(call->targs.empty() && call->args.has_value() && proc.templates != NULL) {
					temps = try_derive_templates(call->targs, proc.params, call->def, proc.templates, __getargs(call->args.value()), proc);
					substituted = true;
				}
				size_t counter {0};
				if(!substituted) {
                    // --- FIX START ---
					if (proc.templates != NULL && call->targs.size() != proc.templates->size()) {
						GeneratorError(call->def, "procedure `" + call->name + "` expects " + std::to_string(proc.templates->size()) + " template arguments, but got " + std::to_string(call->targs.size()));
					}
                    // --- FIX END ---
					for(auto&& el : *proc.templates) {
						temps[el] = call->targs[counter++];
					}
				}
				substitute_template_wct(proc.rettype, temps);
				return proc.rettype;
			}
			if(std::holds_alternative<NodeTermIdent*>(term->var)) {
				std::optional<Var> svar = var_lookup(std::get<NodeTermIdent*>(term->var)->ident.value.value());
				if(svar.has_value()) {
					return svar.value().type;
				}
				std::optional<GVar> glvar = gvar_lookup(std::get<NodeTermIdent*>(term->var)->ident.value.value());
				if(glvar.has_value()) {
					return glvar.value().type;
				}
				std::optional<Constant> scns = const_lookup(std::get<NodeTermIdent*>(term->var)->ident.value.value());
				if(scns.has_value()) {
					return BaseDataTypeInt;
				}
				std::optional<Procedure> prc = proc_lookup(std::get<NodeTermIdent*>(term->var)->ident.value.value());
				if(prc.has_value()) {
					DataType tp = BaseDataTypeProcPtr;
					tp.list.insert_right(prc.value().rettype.root(), tp.list.get_root());
					return tp;
				}
				GeneratorError(std::get<NodeTermIdent*>(term->var)->ident, "unkown word `" + std::get<NodeTermIdent*>(term->var)->ident.value.value() + "`");
			}
		}
		if(holds_alternative<NodeBinExpr*>(expr->var)) {
			NodeBinExpr* binex = std::get<NodeBinExpr*>(expr->var);
			if(std::holds_alternative<NodeBinExprAdd*>(binex->var)) {
				auto add = std::get<NodeBinExprAdd*>(binex->var);
				DataType tp = type_of_expr(add->lhs);
				if(tp.is_object()) {
					NodeTermMtCall mtadd;
					mtadd.def = binex->def;
					mtadd.mt = add->lhs;
					mtadd.name = "m_add";
					std::vector<NodeExpr*> args;
					args.push_back(add->lhs);
					args.push_back(add->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = binex->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					mtadd.args = &eargs;
					NodeTerm asterm { .var = &mtadd };
					NodeExpr asexpr { .var = &asterm };
					return type_of_expr(&asexpr);
				}
				return type_of_expr(add->lhs);
			}
			if(std::holds_alternative<NodeBinExprMulti*>(binex->var)) {
				auto mul = std::get<NodeBinExprMulti*>(binex->var);
				DataType tp = type_of_expr(mul->lhs);
				if(tp.is_object()) {
					NodeTermMtCall mtmul;
					mtmul.def = binex->def;
					mtmul.mt = mul->lhs;
					mtmul.name = "m_mul";
					std::vector<NodeExpr*> args;
					args.push_back(mul->lhs);
					args.push_back(mul->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = binex->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					mtmul.args = &eargs;
					NodeTerm asterm { .var = &mtmul };
					NodeExpr asexpr { .var = &asterm };
					return type_of_expr(&asexpr);
				}
				return type_of_expr(mul->lhs);
			}
			if(std::holds_alternative<NodeBinExprSub*>(binex->var)) {
				auto sub = std::get<NodeBinExprSub*>(binex->var);
				DataType tp = type_of_expr(sub->lhs);
				if(tp.is_object()) {
					NodeTermMtCall mtsub;
					mtsub.def = binex->def;
					mtsub.mt = sub->lhs;
					mtsub.name = "m_sub";
					std::vector<NodeExpr*> args;
					args.push_back(sub->lhs);
					args.push_back(sub->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = binex->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					mtsub.args = &eargs;
					NodeTerm asterm { .var = &mtsub };
					NodeExpr asexpr { .var = &asterm };
					return type_of_expr(&asexpr);
				}
				return type_of_expr(sub->lhs);
			}
			if(std::holds_alternative<NodeBinExprDiv*>(binex->var)) {
				auto div = std::get<NodeBinExprDiv*>(binex->var);
				DataType tp = type_of_expr(div->lhs);
				if(tp.is_object()) {
					NodeTermMtCall mtdiv;
					mtdiv.def = binex->def;
					mtdiv.mt = div->lhs;
					mtdiv.name = "m_div";
					std::vector<NodeExpr*> args;
					args.push_back(div->lhs);
					args.push_back(div->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = binex->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					mtdiv.args = &eargs;
					NodeTerm asterm { .var = &mtdiv };
					NodeExpr asexpr { .var = &asterm };
					return type_of_expr(&asexpr);
				}
				return type_of_expr(div->lhs);
			}
			if(std::holds_alternative<NodeBinExprMod*>(binex->var)) {
				return type_of_expr(std::get<NodeBinExprMod*>(binex->var)->lhs);
			}
			if(std::holds_alternative<NodeBinExprEqEq*>(binex->var)) {
				auto eqeq = std::get<NodeBinExprEqEq*>(binex->var);
				DataType tp = type_of_expr(eqeq->lhs);
				if(tp.is_object()) {
					NodeTermMtCall mteqeq;
					mteqeq.def = binex->def;
					mteqeq.mt = eqeq->lhs;
					mteqeq.name = "m_equal";
					std::vector<NodeExpr*> args;
					args.push_back(eqeq->lhs);
					args.push_back(eqeq->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = binex->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					mteqeq.args = &eargs;
					NodeTerm asterm { .var = &mteqeq };
					NodeExpr asexpr { .var = &asterm };
					return type_of_expr(&asexpr);
				}
				return BaseDataTypeInt;
			}
			if(std::holds_alternative<NodeBinExprNotEq*>(binex->var)) {
				auto nq = std::get<NodeBinExprNotEq*>(binex->var);
				DataType tp = type_of_expr(nq->lhs);
				if(tp.is_object()) {
					NodeTermMtCall mnq;
					mnq.def = binex->def;
					mnq.mt = nq->lhs;
					mnq.name = "m_not_equal";
					std::vector<NodeExpr*> args;
					args.push_back(nq->lhs);
					args.push_back(nq->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = binex->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					mnq.args = &eargs;
					NodeTerm asterm { .var = &mnq };
					NodeExpr asexpr { .var = &asterm };
					return type_of_expr(&asexpr);
				}
				return BaseDataTypeInt;
			}
			if(std::holds_alternative<NodeBinExprLess*>(binex->var)) {
				auto less = std::get<NodeBinExprLess*>(binex->var);
				DataType tp = type_of_expr(less->lhs);
				if(tp.is_object()) {
					NodeTermMtCall mtless;
					mtless.def = binex->def;
					mtless.mt = less->lhs;
					mtless.name = "m_less";
					std::vector<NodeExpr*> args;
					args.push_back(less->lhs);
					args.push_back(less->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = binex->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					mtless.args = &eargs;
					NodeTerm asterm { .var = &mtless };
					NodeExpr asexpr { .var = &asterm };
					return type_of_expr(&asexpr);
				}
				return type_of_expr(less->lhs);
			}
			if(std::holds_alternative<NodeBinExprAbove*>(binex->var)) {
				auto above = std::get<NodeBinExprAbove*>(binex->var);
				DataType tp = type_of_expr(above->lhs);
				if(tp.is_object()) {
					NodeTermMtCall mtabove;
					mtabove.def = binex->def;
					mtabove.mt = above->lhs;
					mtabove.name = "m_above";
					std::vector<NodeExpr*> args;
					args.push_back(above->lhs);
					args.push_back(above->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = binex->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					mtabove.args = &eargs;
					NodeTerm asterm { .var = &mtabove };
					NodeExpr asexpr { .var = &asterm };
					return type_of_expr(&asexpr);
				}
				return type_of_expr(above->lhs);
			}
			if(std::holds_alternative<NodeBinExprAnd*>(binex->var)) {
				return type_of_expr(std::get<NodeBinExprAnd*>(binex->var)->lhs);
			}
			if(std::holds_alternative<NodeBinExprOr*>(binex->var)) {
				return type_of_expr(std::get<NodeBinExprOr*>(binex->var)->lhs);
			}
			if(std::holds_alternative<NodeBinExprShr*>(binex->var)) {
				auto shr = std::get<NodeBinExprShr*>(binex->var);
				DataType tp = type_of_expr(shr->lhs);
				if(tp.is_object()) {
					NodeTermMtCall mtshr;
					mtshr.def = binex->def;
					mtshr.mt = shr->lhs;
					mtshr.name = "m_shr";
					std::vector<NodeExpr*> args;
					args.push_back(shr->lhs);
					args.push_back(shr->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = binex->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					mtshr.args = &eargs;
					NodeTerm asterm { .var = &mtshr };
					NodeExpr asexpr { .var = &asterm };
					return type_of_expr(&asexpr);
				}
				return type_of_expr(shr->lhs);
			}
			if(std::holds_alternative<NodeBinExprShl*>(binex->var)) {
				auto shl = std::get<NodeBinExprShl*>(binex->var);
				DataType tp = type_of_expr(shl->lhs);
				if(tp.is_object()) {
					NodeTermMtCall mtshl;
					mtshl.def = binex->def;
					mtshl.mt = shl->lhs;
					mtshl.name = "m_shl";
					std::vector<NodeExpr*> args;
					args.push_back(shl->lhs);
					args.push_back(shl->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = binex->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					mtshl.args = &eargs;
					NodeTerm asterm { .var = &mtshl };
					NodeExpr asexpr { .var = &asterm };
					return type_of_expr(&asexpr);
				}
				return type_of_expr(shl->lhs);
			}
			if(std::holds_alternative<NodeBinExprDot*>(binex->var)) {
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
		size_t counter {0};
		for(auto&& el : templates) {
			temps[el] = targs[counter++];
		}
		return temps;
	}

	__map<std::string, Field> compute_fields(const std::vector<std::pair<std::string, DataType>>& fields) {
		__map<std::string, Field> __fields;
		size_t nth = 0ULL;
		for(const std::pair<std::string, DataType>& field : fields) {
			__fields[field.first] = { .name = field.first, .type = field.second, .nth = nth++ };
		}
		return __fields;
	}

	void gen_push_str(__str_ref value) {
		std::optional<String> str = string_lookup(value);
		size_t _index = 0;
		if(!str.has_value()) {
			size_t index = (*m_string_index)++;
			m_strings[value] = { .value = value, .index = index };
			_index = index;
		} else {
			_index = str.value().index;
		}
		m_output << "    push s_" << _index << '\n';
	}

	void gen_traceback_push(Procedure& proc) {
		gen_push_str(proc.name);
		m_output << "    call traceback_push\n";
		m_output << "    add esp, 4\n";
	}

	void gen_traceback_push_nm(Procedure& proc, __str_ref nm) {
		gen_push_str(nm + "::" + proc.name);
		m_output << "    call traceback_push\n";
		m_output << "    add esp, 4\n";
	}

	void gen_term(const NodeTerm* term, bool lvalue = false)
	{
		struct TermVisitor {
			Generator& gen;
			bool lvalue;

			void operator()(const NodeTermIntLit* term_int_lit) const
			{
				if(lvalue) gen.GeneratorError(term_int_lit->int_lit, "can't use integer constant as lvalue expression (any operations that taking addres).");
				gen.push(term_int_lit->int_lit.value.value());
			}

			void operator()(const NodeTermType* tp) {
				gen.GeneratorError(tp->def, "`type` only can be used in context of compile-time expressions");
			}

			void operator()(const NodeTermCol* term_col) const
			{
				gen.m_output << "push " << term_col->def.col << "\n";
			}

			void operator()(const NodeTermLine* term_line) const
			{
				gen.m_output << "push " << term_line->def.line << "\n";
			}

			void operator()(const NodeTermPop* term_pop) const
			{
				consume_un(term_pop);
			}

			void operator()(const NodeTermExprStmt* term_stmt) const
			{
				gen.gen_scope(term_stmt->scope);
				gen.gen_expr(term_stmt->expr);
			}

			void operator()(const NodeTermFile* term_file) const
			{
				std::string value = term_file->def.file;
				std::optional<String> str = gen.string_lookup(value);
				if(!str.has_value()) {
					size_t index = (*gen.m_string_index)++;
					gen.m_strings[value] = { .value = value, .index = index};
					gen.m_output << "    push s_" << index << "\n";
					return;
				}
				gen.m_output << "    push s_" << str.value().index << "\n";
			}

			void operator()(const NodeTermCtEval* term_eval) const
			{
				gen.GeneratorError(term_eval->def, "ct_eval is deprecated");
			}

			void operator()(const NodeTermCtMdefined* term_mdef) const
			{
				gen.m_output << "    push " << static_cast<int>(term_mdef->value) << "\n";
			}

			void operator()(NodeTermSizeof* term_sizeof) const
			{
				if(term_sizeof->expr.has_value()) {
					DataType tp = gen.type_of_expr(term_sizeof->expr.value());
					if(size_t size = gen.sizeof_of(term_sizeof->type)) {
						gen.m_output << "    push " << size << "\n";
					} else {
						gen.GeneratorError(term_sizeof->def, "please provide a type to sizeof, or close expression to parens.");
					}
				}
				else {
					DataType dt = term_sizeof->type;
					gen.substitute_template(dt);
					if(size_t size = gen.sizeof_of(dt)) {
						gen.m_output << "    push " << size << "\n";
					} else {
						gen.GeneratorError(term_sizeof->def, "please provide a type to sizeof, or close expression to parens.");
					}
				}
			}

			void operator()(const NodeTermRd* term_rd) const
			{
				if(term_rd->size == 8) {
					gen.gen_expr(term_rd->expr);
					gen.m_output << "    pop edx\n";
					gen.m_output << "    xor ecx, ecx\n";
					gen.m_output << "    mov cl, byte [edx]\n";
					gen.m_output << "    push ecx\n";
				} else if(term_rd->size == 16) {
					gen.gen_expr(term_rd->expr);
					gen.m_output << "    pop edx\n";
					gen.m_output << "    xor ecx, ecx\n";
					gen.m_output << "    mov cx, word [edx]\n";
					gen.m_output << "    push ecx\n";
				} else if(term_rd->size == 32) {
					gen.gen_expr(term_rd->expr);
					gen.m_output << "    pop edx\n";
					gen.m_output << "    push dword [edx]\n";
				} else {
					assert(false); // unreacheable
				}
			}

			void operator()(const NodeTermCast* term_cast) const
			{
				gen.gen_expr(term_cast->expr, lvalue);
			}

			void operator()(const NodeTermUnref* term_unref) const
			{
				DataType tp = gen.type_of_expr(term_unref->expr);
				if(tp.is_object() && !tp.root().link && tp.root().ptrlvl == 0) {
					if(lvalue) {
						NodeTermMtCall deref;
						deref.def = term_unref->def;
						deref.mt = term_unref->expr;
						deref.name = "m_assign_deref";
						std::vector<NodeExpr*> args;
						args.push_back(term_unref->expr);
						NodeBinExprArgs bargs { .args = args};
						NodeBinExpr bexpr { .def = term_unref->def , .var = &bargs };
						NodeExpr eargs{ .var = &bexpr };
						deref.args = &eargs;
						NodeTerm asterm { .var = &deref };
						NodeExpr asexpr { .var = &asterm };
						gen.gen_expr(&asexpr);
					} else {
						NodeTermMtCall deref;
						deref.def = term_unref->def;
						deref.mt = term_unref->expr;
						deref.name = "m_deref";
						std::vector<NodeExpr*> args;
						args.push_back(term_unref->expr);
						NodeBinExprArgs bargs { .args = args};
						NodeBinExpr bexpr { .def = term_unref->def , .var = &bargs };
						NodeExpr eargs{ .var = &bexpr };
						deref.args = &eargs;
						NodeTerm asterm { .var = &deref };
						NodeExpr asexpr { .var = &asterm };
						gen.gen_expr(&asexpr);
					}
					return;
				}
				gen.gen_expr(term_unref->expr, lvalue);
				gen.m_output << "    pop edx\n";
				gen.m_output << "    push dword [edx]\n";
			}

			void operator()(const NodeTermCastTo* term_cast_to) const
			{
				gen.gen_expr(term_cast_to->expr, lvalue);
			}

			void operator()(NodeTermTypeid* term_typeid) const
			{
				if(term_typeid->ptype.has_value()) {
					gen.m_output << "    push dword " << gen.typeid_of(term_typeid->ptype.value()) << "\n";
				}
				else {
					gen.m_output << "    push dword " << gen.typeid_of(gen.type_of_expr(term_typeid->expr)) << "\n";
				}
			}

			void operator()(const NodeTermStrLit* term_str_lit) const
			{
				__str_ref value = term_str_lit->str_lit;
				std::optional<String> str = gen.string_lookup(value);
				if(!str.has_value()) {
					size_t index = (*gen.m_string_index)++;
					gen.m_strings[value] = { .value = value, .index = index };
					gen.m_output << "    push s_" << index << "\n";
					return;
				}
				gen.m_output << "    push s_" << str.value().index << "\n";
			}

			void operator()(const NodeTermAmpersand* term_amp) const
			{
				gen.gen_expr(term_amp->expr, true);
			}

			void operator()(const NodeTermDrvalue* term_drval) const
			{
				if (lvalue) {
					gen.GeneratorError(term_drval->def, "using __disable_rvalue__ as rvalue");
				}
				gen.gen_expr(term_drval->expr, false);
			}

			void operator()(const NodeTermIdent* term_ident) const
			{
				std::optional<Var> it = gen.var_lookup(term_ident->ident.value.value());
				if(it.has_value()) {
					if(lvalue) {
						gen.m_output << "    mov edx, ebp\n";
						gen.m_output << "    sub edx, " << it.value().stack_loc << "\n";
						gen.m_output << "    push edx\n";
					} else {
						std::stringstream offset;
						offset << "dword [ebp-" << it.value().stack_loc << "]";
						gen.push(offset.str());
					}
					return;
				}
				std::optional<Constant> cns = gen.const_lookup(term_ident->ident.value.value());
				if(cns.has_value()) {
					if(lvalue) {
						gen.GeneratorError(term_ident->ident, "can't get addres or reference of compile-time constant.");
					}
					gen.m_output << "    push dword " << cns.value().value << "\n";
					return;
				}
				std::optional<Procedure> prc = gen.proc_lookup(term_ident->ident.value.value());
				if(prc.has_value()) {
					gen.m_output << "    push dword " << prc.value().name << "\n";
					return;
				}
				std::optional<GVar> ivar = gen.gvar_lookup(term_ident->ident.value.value());
				if(ivar.has_value()) {
					if(lvalue) {
						gen.m_output << "    push dword v_" << ivar.value().name << "\n";
					}
					else {
						gen.m_output << "    push dword [v_" << ivar.value().name << "]\n";
					}
					return;
				}
				gen.GeneratorError(term_ident->ident, "unkown word `" + term_ident->ident.value.value() + "`");
			}

			void operator()(const NodeTermParen* term_paren) const
			{
				gen.gen_expr(term_paren->expr, lvalue);
			}

			void operator()(NodeTermNmCall* term_call) const
			{
				__str_ref pname = term_call->name;
				__str_ref nname = term_call->nm;
				std::optional<Namespace*> nms = gen.namespace_lookup(nname);
				if(!nms.has_value()) gen.GeneratorError(term_call->def, "unkown namespace `" + nname + "`");
				const auto& search = nms.value()->procs.find(pname);
				if(search == nms.value()->procs.end()) gen.GeneratorError(term_call->def, "namespace `" + nname + "` doesn't have procedure `" + pname + "`");
				Procedure proc = search->second;
				if(!proc.overrides.empty()) {
					gen.resolve_overrides_tp(&proc, term_call->args, term_call->def, term_call->targs);
				}
				__map<std::string, DataType> temps;
				bool substituted = false;
				if(proc.templates != NULL && term_call->targs.empty()) {
					if(term_call->args.has_value()) {
						temps = gen.try_derive_templates(term_call->targs, proc.params, term_call->def, proc.templates, gen.__getargs(term_call->args.value()), proc);
						substituted = true;
					}
				}
				if(proc.templates != NULL && term_call->targs.empty()) {
					gen.GeneratorError(term_call->def, "procedure `" + term_call->name + "` excepts template arguments in <...>.");
				}
				std::string tsign;
				if (!term_call->targs.empty()) {
				    for (int i = 0; i < static_cast<int>(term_call->targs.size()); ++i) {
				        DataType t = term_call->targs[i];   // копия
       					gen.substitute_template(t);
        				tsign += t.sign();
				    }
				
				    // Берём именно тот Procedure, который реально выбран оверлоад-резолвером
				    Procedure* inst_p = proc.override
				        ? gen.m_namespaces[nname]->procs[pname].overrides[proc.overload_nth - 1]
				        : &gen.m_namespaces[nname]->procs[pname];
				
				    if (!inst_p->instanceated[tsign]) {
				        Generator dop_gen(gen);
				
				        dop_gen.m_temps.emplace_back(__map<std::string, DataType>{});
				        size_t counter {0};
				        for (auto&& el : *proc.templates) {
				            dop_gen.m_temps.back()[el] = term_call->targs[counter++];
				        }
				
				        if (substituted)
				            gen.substitute_template_params(temps, proc.params);
				        else
				            gen.substitute_template_params(dop_gen.m_temps.back(), proc.params);
				
				        inst_p->instanceated[tsign] = true;
				
				        dop_gen.m_output << nname << "@" << proc.name << tsign;
				        if (proc.override)
				            dop_gen.m_output << proc.get_sign(); // ВАЖНО: различать перегрузки
				        dop_gen.m_output << ":\n";
				
				        dop_gen.m_output << "    push ebp\n";
				        dop_gen.m_output << "    mov ebp, esp\n";
				        dop_gen.gen_traceback_push_nm(proc, nname);
				        dop_gen.m_tsigns.push_back(tsign);
				        proc.mbn = nname;
				        dop_gen.substitute_template(proc.rettype);
				        dop_gen.m_cur_proc = proc;
				        dop_gen.gen_scope_sp(proc.scope, proc.from, proc);
						if(!substituted) {
							temps = std::move(dop_gen.m_temps.back());
						}
						gen.m_strings = std::move(dop_gen.m_strings);
						dop_gen.m_temps.pop_back();
						dop_gen.m_tsigns.pop_back();
						dop_gen.m_output << "    push eax\n";
						dop_gen.m_output << "    call traceback_pop\n";
						dop_gen.m_output << "    pop eax\n";
						dop_gen.m_output << "    pop ebp\n";
						dop_gen.m_output << "    ret\n\n";
						(*gen.m_result) << (dop_gen.m_output.str());
					} else {
						if(!substituted) {
							size_t counter {0};
							for(auto&& el : *proc.templates) {
								temps[el] = term_call->targs[counter++];
							}
						}
						gen.substitute_template_params(temps, proc.params);
					}
				}
				if(proc.rettype.root() == BaseDataTypeVoid) gen.GeneratorError(term_call->def, "can't use void " + nname + "::" + pname + "(...) as value");
				size_t stack_allign = 0;
				if(term_call->args.has_value()) {
					gen.__typecheck_call(gen.__getargs(term_call->args.value()), proc.params, term_call->def, proc, &stack_allign);
				} else if(proc.params.size() != 0ULL) {
					gen.GeneratorError(term_call->def, "procedure `" + proc.name + "` excepts " + std::to_string(proc.params.size()) + " arguments\nNOTE: but got 0");
				}
				if(term_call->args.has_value()) {
					gen.gen_args(gen.__getargs(term_call->args.value()), proc.params);
				}
				gen.m_output << "    call " << nname << "@" << pname;
				if (!term_call->targs.empty())
				    gen.m_output << tsign;
				if (proc.override)
				    gen.m_output << proc.get_sign();
				gen.m_output << "\n";
				if(stack_allign != 0) {
					gen.m_output << "    add esp, " << stack_allign * 4 << "\n";
				}
				gen.m_output << "    push eax\n";
			}

			void operator()(NodeTermCall* term_call) const
			{
				const std::string name = term_call->def.value.value();
				std::optional<Procedure> _proc = gen.proc_lookup(name);
				if(_proc.has_value()) {
					Procedure proc = _proc.value();
					if(proc.rettype == BaseDataTypeConst) {
						int res {0};
						Executor executor(gen, term_call->def);
						try {
							executor.execute(proc);
						} catch(Executor::ReturnException& ret) {
							res = ret.get_value();
						}
						gen.m_output << "    push dword " << res << std::endl;
						return;
					}
					if(!proc.overrides.empty()) {
						gen.resolve_overrides_tp(&proc, term_call->args, term_call->def, term_call->targs);
					}
					if(proc.rettype.root() == BaseDataTypeVoid) gen.GeneratorError(term_call->def, "can't use void " + term_call->name + "(...) as value");
					__map<std::string, DataType> temps;
					bool substituted = false;
					if(proc.templates != NULL && term_call->targs.empty()) {
						if(term_call->args.has_value()) {
							temps = gen.try_derive_templates(term_call->targs, proc.params, term_call->def, proc.templates, gen.__getargs(term_call->args.value()), proc);
							substituted = true;
						}
					}
					if(proc.templates != NULL && term_call->targs.empty()) {
						gen.GeneratorError(term_call->def, "procedure `" + term_call->name + "` excepts template arguments in <...>.");
					}
					std::string tsign;
					if(!term_call->targs.empty()) {
						for(int i = 0;i < static_cast<int>(term_call->targs.size());++i) {
							DataType t = term_call->targs[i];   // копия
        					gen.substitute_template(t);
        					tsign += t.sign();
						}
						if(!proc.instanceated[tsign]) {
							Generator dop_gen(gen);
							gen.m_procs[term_call->name].instanceated[tsign] = true;
							dop_gen.m_output << proc.name << tsign;
							dop_gen.m_output << ":\n";
							dop_gen.m_output << "    push ebp\n";
							dop_gen.m_output << "    mov ebp, esp\n";
							dop_gen.gen_traceback_push(proc);
							dop_gen.m_tsigns.push_back(tsign);
							size_t counter {0};
							dop_gen.m_temps.emplace_back(__map<std::string, DataType>{});
							for(auto&& el : *proc.templates) {
								dop_gen.m_temps[dop_gen.m_temps.size() - 1][el] = term_call->targs[counter++];
							}
							if(substituted) gen.substitute_template_params(temps, proc.params);
							else gen.substitute_template_params(dop_gen.m_temps.back(), proc.params);
							dop_gen.substitute_template(proc.rettype);
							dop_gen.m_cur_proc = proc;
							dop_gen.gen_scope_sp(proc.scope, proc.from, proc);
							if(!substituted)
								temps = std::move(dop_gen.m_temps.back());
							gen.m_strings = std::move(dop_gen.m_strings);
							dop_gen.m_temps.pop_back();
							dop_gen.m_tsigns.pop_back();
							dop_gen.m_output << "    push eax\n";
							dop_gen.m_output << "    call traceback_pop\n";
							dop_gen.m_output << "    pop eax\n";
							dop_gen.m_output << "    pop ebp\n";
							dop_gen.m_output << "    ret\n\n";
							(*gen.m_result) << (dop_gen.m_output.str());
						}
					}
					size_t stack_allign = 0;
					if(term_call->args.has_value()) gen.__typecheck_call(gen.__getargs(term_call->args.value()), proc.params, term_call->def, proc, &stack_allign);
					else if(proc.params.size() != 0ULL) gen.GeneratorError(term_call->def, "procedure `" + proc.name + "` excepts " + std::to_string(proc.params.size()) + " arguments\nNOTE: but got 0");
					if(term_call->args.has_value()) gen.gen_args(gen.__getargs(term_call->args.value()), proc.params);
					gen.m_output << "    call " << name;
					if(proc.override) gen.m_output << proc.get_sign();
					if(!term_call->targs.empty()) gen.m_output << tsign;
					gen.m_output << "\n";
					if(stack_allign != 0) gen.m_output << "    add esp, " << stack_allign * 4 << "\n";
					gen.m_output << "    push eax\n";
					return;
				}
				std::optional<Struct> st = gen.struct_lookup(term_call->name);
				if(st.has_value()) {
					if(st.value().temp && term_call->targs.size() != st.value().temps.size()) gen.GeneratorError(term_call->def, "struct `" + st.value().name + "` except " + std::to_string(st.value().temps.size()) + " template arguments in <...>, bug got " + std::to_string(term_call->targs.size()) + ".");
					size_t objectSize = st.value().fields.size();
					if(objectSize == 0U) {
						gen.GeneratorError(term_call->def, "try to allocate zero-sized type.");
					}
					gen.m_output << "    push dword " << objectSize * 4U << "\n";
					if(st.value().has_allocator()) {
						gen.m_output << "    call " << st.value().__allocator.value() << "\n";
					}
					else { 
						gen.m_output << "    call memalloc\n";
					}
					gen.m_output << "    add esp, 4\n";
					bool eax_break = false;
					std::vector<NodeExpr*> iargs;
					if(term_call->args.has_value()) {
						if(std::holds_alternative<NodeBinExpr*>(term_call->args.value()->var)) {
							NodeBinExpr* binargs = std::get<NodeBinExpr*>(term_call->args.value()->var);
							if(std::holds_alternative<NodeBinExprArgs*>(binargs->var)) {
								NodeBinExprArgs* args = std::get<NodeBinExprArgs*>(binargs->var);
								iargs = args->args;
							} else {
								iargs.push_back(term_call->args.value());
							}
						} else {
							if(iargs.size() == 0U) {
								iargs.push_back(term_call->args.value());
							}
						}
					}
					if(iargs.size() != 0U) {
						if(iargs.size() != st.value().fields.size()) {
							gen.GeneratorError(term_call->def, "except " + std::to_string(st.value().fields.size()) + " args\nNOTE: but got " + std::to_string(iargs.size()) + "\nNOTE: if you don't want initialize all fields dont provide any arguments");
						}
						eax_break = true;
						gen.m_output << "    mov edx, dword [tmp_p]\n";
						gen.m_output << "    add edx, 4\n";
						gen.m_output << "    mov dword [tmp_p], edx\n";
						gen.m_output << "    mov dword [tmp_stor+edx], eax\n";
						Struct _st = st.value();
						__map<std::string, DataType> temps;
						if(_st.temp) {
							size_t counter {0};
							for(auto&& el : _st.temps) {
								temps[el] = term_call->targs[counter++];
							}
						}
						for(int i = 0;i < static_cast<int>(iargs.size());++i) {
							DataType itype = gen.type_of_expr(iargs[i]);
							Field f;
							DataType ftype = _st.__fields[i].second;
							if(_st.temp)
								gen.substitute_template_wct(ftype, temps);
							if(itype != ftype) {
								gen.GeneratorError(term_call->def, "missmatch in initializers types for field nth `" + std::to_string(i + 1) + "`\nNOTE: field name - `" + _st.__fields[i].first + "`" + "\nNOTE: excepted " + ftype.to_string() + "\nNOTE: but got " + itype.to_string());
							}
							gen.gen_expr(iargs[i]);
							gen.m_output << "    pop ecx\n";
							gen.m_output << "    mov ebx, dword [tmp_p]\n";
							gen.m_output << "    mov edx, dword [tmp_stor+ebx]\n";
							gen.m_output << "    mov dword [edx+" << i * 4 << "], ecx\n";
						}
					}
					if(!eax_break) {
						gen.m_output << "    push eax\n";
					} else {
						gen.m_output << "    mov edx, dword [tmp_p]\n";
						gen.m_output << "    push dword [tmp_stor+edx]\n";
						gen.m_output << "    sub edx, 4\n";
						gen.m_output << "    mov dword [tmp_p], edx\n";
					}
					return;
				}
				gen.GeneratorError(term_call->def, "unkown procedure `" + name + "`");
			}

			void operator()(const NodeTermMtCall* term_call) const {
				DataType tpof = gen.type_of_expr(term_call->mt);
				if(!tpof.root().is_object || tpof.root().link) gen.GeneratorError(term_call->def, "can't call method from type " + tpof.to_string() + ".");
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
		TermVisitor visitor({ .gen = *this , .lvalue = lvalue });
		std::visit(visitor, term->var);
	}

	void gen_bin_expr(const NodeBinExpr* bin_expr, bool lvalue = false)
	{
		struct BinExprVisitor {
			Generator& gen;
			bool lvalue;
			const NodeBinExpr* base;

			void operator()(const NodeBinExprSub* sub) const
			{
				DataType oneT = gen.type_of_expr(sub->lhs);
				DataType twoT = gen.type_of_expr(sub->rhs);
				if(oneT.root().is_object) {
					NodeTermNmCall sb;
					sb.def = base->def;
					sb.nm = "std";
					sb.name = "sub";
					std::vector<NodeExpr*> args;
					args.push_back(sub->lhs);
					args.push_back(sub->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = base->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					sb.args = &eargs;
					NodeTerm asterm { .var = &sb };
					NodeExpr asexpr { .var = &asterm };
					gen.gen_expr(&asexpr);
					return;
				}

				if(oneT != twoT && !(oneT.root() == BaseDataTypePtr && twoT.root() == BaseDataTypeInt)) {
					gen.GeneratorError(base->def, "can't use operator - for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
				}
				gen.gen_expr(sub->rhs);
				gen.gen_expr(sub->lhs);
				gen.pop("eax");
				gen.pop("ebx");
				gen.m_output << "    sub eax, ebx\n";
				gen.push("eax");
			}

			void operator()(const NodeBinExprAdd* add) const
			{
				DataType oneT = gen.type_of_expr(add->lhs);
				DataType twoT = gen.type_of_expr(add->rhs);
				if(oneT.root().is_object) {
					NodeTermMtCall dd;
					dd.def = base->def;
					dd.mt = add->lhs;
					dd.name = "m_add";
					std::vector<NodeExpr*> args;
					args.push_back(add->lhs);
					args.push_back(add->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = base->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					dd.args = &eargs;
					NodeTerm asterm { .var = &dd };
					NodeExpr asexpr { .var = &asterm };
					gen.gen_expr(&asexpr);
					return;
				}

				if(oneT != twoT && !(oneT.root() == BaseDataTypePtr && twoT.root() == BaseDataTypeInt) && !(oneT.root().ptrlvl != 0ULL && twoT.root() == BaseDataTypeInt)) gen.GeneratorError(base->def, "can't use operator + for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
				gen.gen_expr(add->rhs);
				gen.gen_expr(add->lhs);
				gen.pop("eax");
				gen.pop("ebx");
				gen.m_output << "    add eax, ebx\n";
				gen.push("eax");
			}

			void operator()(const NodeBinExprMulti* multi) const
			{
				DataType oneT = gen.type_of_expr(multi->lhs);
				DataType twoT = gen.type_of_expr(multi->rhs);
				if(oneT.root().is_object) {
					NodeTermMtCall dd;
					dd.def = base->def;
					dd.mt = multi->lhs;
					dd.name = "m_mul";
					std::vector<NodeExpr*> args;
					args.push_back(multi->lhs);
					args.push_back(multi->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = base->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					dd.args = &eargs;
					NodeTerm asterm { .var = &dd };
					NodeExpr asexpr { .var = &asterm };
					gen.gen_expr(&asexpr);
					return;
				}

				if(oneT != twoT) gen.GeneratorError(base->def, "can't use operator * for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
				gen.gen_expr(multi->rhs);
				gen.gen_expr(multi->lhs);
				gen.pop("eax");
				gen.pop("ebx");
				gen.m_output << "    mul ebx\n";
				gen.push("eax");
			}

			void operator()(const NodeBinExprDiv* div) const
			{
				DataType oneT = gen.type_of_expr(div->lhs);
				DataType twoT = gen.type_of_expr(div->rhs);
				if(oneT.root().is_object) {
					NodeTermMtCall dd;
					dd.def = base->def;
					dd.mt = div->lhs;
					dd.name = "m_div";
					std::vector<NodeExpr*> args;
					args.push_back(div->lhs);
					args.push_back(div->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = base->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					dd.args = &eargs;
					NodeTerm asterm { .var = &dd };
					NodeExpr asexpr { .var = &asterm };
					gen.gen_expr(&asexpr);
					return;
				}

				if(oneT != twoT) gen.GeneratorError(base->def, "can't use operator / for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
				gen.gen_expr(div->rhs);
				gen.gen_expr(div->lhs);
				gen.pop("eax");
				gen.pop("ebx");
				gen.m_output << "    xor edx, edx\n";
				gen.m_output << "    div ebx\n";
				gen.push("eax");
				gen.m_output << "    mov edx, ecx\n";
			}

			void operator()(const NodeBinExprShl* shl) const
			{
				DataType oneT = gen.type_of_expr(shl->lhs);
				DataType twoT = gen.type_of_expr(shl->rhs);
				if(oneT.root().is_object) {
					NodeTermMtCall dd;
					dd.def = base->def;
					dd.mt = shl->lhs;
					dd.name = "m_shl";
					std::vector<NodeExpr*> args;
					args.push_back(shl->lhs);
					args.push_back(shl->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = base->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					dd.args = &eargs;
					NodeTerm asterm { .var = &dd };
					NodeExpr asexpr { .var = &asterm };
					gen.gen_expr(&asexpr);
					return;
				}

				if(oneT != twoT) gen.GeneratorError(base->def, "can't use operator << for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
				gen.gen_expr(shl->rhs);
				gen.gen_expr(shl->lhs);
				gen.pop("eax");
				gen.pop("ecx");
				gen.m_output << "    shl eax, cl\n";
				gen.push("eax");
			}

			void operator()(const NodeBinExprShr* shr) const
			{
				DataType oneT = gen.type_of_expr(shr->lhs);
				DataType twoT = gen.type_of_expr(shr->rhs);
				if(oneT.root().is_object) {
					NodeTermMtCall dd;
					dd.def = base->def;
					dd.mt = shr->lhs;
					dd.name = "m_shr";
					std::vector<NodeExpr*> args;
					args.push_back(shr->lhs);
					args.push_back(shr->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = base->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					dd.args = &eargs;
					NodeTerm asterm { .var = &dd };
					NodeExpr asexpr { .var = &asterm };
					gen.gen_expr(&asexpr);
					return;
				}

				if(oneT != twoT) gen.GeneratorError(base->def, "can't use operator >> for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
				gen.gen_expr(shr->rhs);
				gen.gen_expr(shr->lhs);
				gen.pop("eax");
				gen.pop("ecx");
				gen.m_output << "    shr eax, cl\n";
				gen.push("eax");
			}

			void operator()(const NodeBinExprMod* md) const
			{
				DataType oneT = gen.type_of_expr(md->lhs);
				DataType twoT = gen.type_of_expr(md->rhs);
				if(oneT.root().is_object) {
					NodeTermNmCall m;
					m.def = base->def;
					m.nm = "std";
					m.name = "mod";
					std::vector<NodeExpr*> args;
					args.push_back(md->lhs);
					args.push_back(md->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = base->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					m.args = &eargs;
					NodeTerm asterm { .var = &m };
					NodeExpr asexpr { .var = &asterm };
					gen.gen_expr(&asexpr);
					return;
				}

				if(oneT != twoT) gen.GeneratorError(base->def, "can't use operator % for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
				gen.gen_expr(md->rhs);
				gen.gen_expr(md->lhs);
				gen.pop("eax");
				gen.pop("ebx");
				gen.m_output << "    xor edx, edx\n";
				gen.m_output << "    div ebx\n";
				gen.push("edx");
			}

			void operator()(const NodeBinExprEqEq* eqeq) const
			{
				DataType oneT = gen.type_of_expr(eqeq->lhs);
				DataType twoT = gen.type_of_expr(eqeq->rhs);
				if(oneT.root().is_object) {
					NodeTermMtCall equal;
					equal.def = base->def;
					equal.mt = eqeq->lhs;
					equal.name = "m_equal";
					std::vector<NodeExpr*> args;
					args.push_back(eqeq->lhs);
					args.push_back(eqeq->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = base->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					equal.args = &eargs;
					NodeTerm asterm { .var = &equal };
					NodeExpr asexpr { .var = &asterm };
					gen.gen_expr(&asexpr);
					return;
				}

				if(oneT != twoT) gen.GeneratorError(base->def, "can't use operator == for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");

				gen.gen_expr(eqeq->rhs);
				gen.gen_expr(eqeq->lhs);
				gen.m_output << "    mov edx, 0\n";
				gen.m_output << "    mov ecx, 1\n";
				gen.m_output << "    pop ebx\n";
				gen.m_output << "    pop eax\n";
				gen.m_output << "    cmp eax, ebx\n";
				gen.m_output << "    cmove edx, ecx\n";
				gen.m_output << "    push edx\n";
			}

			void operator()(const NodeBinExprNotEq* nq) const
			{
				DataType oneT = gen.type_of_expr(nq->lhs);
				DataType twoT = gen.type_of_expr(nq->rhs);
				if(oneT.root().is_object) {
					NodeTermMtCall nequal;
					nequal.def = base->def;
					nequal.mt = nq->lhs;
					nequal.name = "m_not_equal";
					std::vector<NodeExpr*> args;
					args.push_back(nq->lhs);
					args.push_back(nq->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = base->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					nequal.args = &eargs;
					NodeTerm asterm { .var = &nequal };
					NodeExpr asexpr { .var = &asterm };
					gen.gen_expr(&asexpr);
					return;
				}

				if(oneT != twoT) gen.GeneratorError(base->def, "can't use operator != for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
				gen.gen_expr(nq->rhs);
				gen.gen_expr(nq->lhs);
				gen.m_output << "    mov edx, 0\n";
				gen.m_output << "    mov ecx, 1\n";
				gen.m_output << "    pop ebx\n";
				gen.m_output << "    pop eax\n";
				gen.m_output << "    cmp eax, ebx\n";
				gen.m_output << "    cmovne edx, ecx\n";
				gen.m_output << "    push edx\n";
			}

			void operator()(const NodeBinExprLess* less) const
			{
				DataType oneT = gen.type_of_expr(less->lhs);
				DataType twoT = gen.type_of_expr(less->rhs);
				if(oneT.root().is_object) {
					NodeTermMtCall mtless;
					mtless.def = base->def;
					mtless.mt = less->lhs;
					mtless.name = "m_less";
					std::vector<NodeExpr*> args;
					args.push_back(less->lhs);
					args.push_back(less->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = base->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					mtless.args = &eargs;
					NodeTerm asterm { .var = &mtless };
					NodeExpr asexpr { .var = &asterm };
					gen.gen_expr(&asexpr);
					return;
				}

				if(oneT != twoT) gen.GeneratorError(base->def, "can't use operator < for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
				gen.gen_expr(less->lhs);
				gen.gen_expr(less->rhs);
				gen.m_output << "    mov edx, 0\n";
				gen.m_output << "    mov ecx, 1\n";
				gen.m_output << "    pop ebx\n";
				gen.m_output << "    pop eax\n";
				gen.m_output << "    cmp eax, ebx\n";
				gen.m_output << "    cmovl edx, ecx\n";
				gen.m_output << "    push edx\n";
			}

			void operator()(const NodeBinExprAnd* band) const
			{
				gen.gen_expr(band->lhs);
				const std::string flab = gen.create_label();
				const std::string elab = gen.create_label();
				gen.m_output << "    xor eax, eax\n";
				gen.m_output << "    pop edx\n";
				gen.m_output << "    cmp edx, 0\n";
				gen.m_output << "    jz " << flab << "\n";
				gen.gen_expr(band->rhs);
				gen.m_output << "    pop edx\n";
				gen.m_output << "    cmp edx, 0\n";
				gen.m_output << "    jz " << flab << "\n";
				gen.m_output << "    mov eax, 1\n";
				gen.m_output << "    jmp " << elab << "\n";
				gen.m_output << "    " << flab << ":\n";
				gen.m_output << "    mov eax, 0\n";
				gen.m_output << "    " << elab << ":\n";
				gen.m_output << "    push eax\n";
			}

			void operator()(const NodeBinExprOr* bor) const
			{
				const std::string flab = gen.create_label();
				const std::string elab = gen.create_label();
				const std::string tlab = gen.create_label();
				gen.gen_expr(bor->lhs);
				gen.m_output << "    xor eax, eax\n";
				gen.m_output << "    pop edx\n";
				gen.m_output << "    cmp edx, 0\n";
				gen.m_output << "    jne " << tlab << "\n";
				gen.gen_expr(bor->rhs);
				gen.m_output << "    pop edx\n";
				gen.m_output << "    je " << flab << "\n";
				gen.m_output << "    " << tlab << ":\n";
				gen.m_output << "    mov eax, 1\n";
				gen.m_output << "    jmp " << elab << "\n";
				gen.m_output << "    " << flab << ":\n";
				gen.m_output << "    mov eax, 0\n";
				gen.m_output << "    " << elab << ":\n";
				gen.m_output << "    push eax\n";
			}

			void operator()(const NodeBinExprAbove* above) const
			{
				DataType oneT = gen.type_of_expr(above->lhs);
				DataType twoT = gen.type_of_expr(above->rhs);
				if(oneT.root().is_object) {
					NodeTermMtCall mtabove;
					mtabove.def = base->def;
					mtabove.mt = above->lhs;
					mtabove.name = "m_above";
					std::vector<NodeExpr*> args;
					args.push_back(above->lhs);
					args.push_back(above->rhs);
					NodeBinExprArgs bargs { .args = args};
					NodeBinExpr bexpr { .def = base->def , .var = &bargs };
					NodeExpr eargs{ .var = &bexpr };
					mtabove.args = &eargs;
					NodeTerm asterm { .var = &mtabove };
					NodeExpr asexpr { .var = &asterm };
					gen.gen_expr(&asexpr);
					return;
				}

				if(oneT != twoT) gen.GeneratorError(base->def, "can't use operator == for 2 diffirent types " + oneT.to_string() + " and " + twoT.to_string() + ".");
				gen.gen_expr(above->lhs);
				gen.gen_expr(above->rhs);
				gen.m_output << "    mov edx, 0\n";
				gen.m_output << "    mov ecx, 1\n";
				gen.m_output << "    pop ebx\n";
				gen.m_output << "    pop eax\n";
				gen.m_output << "    cmp eax, ebx\n";
				gen.m_output << "    cmovg edx, ecx\n";
				gen.m_output << "    push edx\n";
			}

			void operator()(const NodeBinExprDot* dot) const
			{
				DataType otype = gen.type_of_expr(dot->lhs);
				if(std::holds_alternative<NodeTerm*>(dot->rhs->var)) {
					NodeTerm* id = std::get<NodeTerm*>(dot->rhs->var);
					if(!std::holds_alternative<NodeTermIdent*>(id->var)) {
						gen.GeneratorError(base->def, "after `.` except identificator");
					}
					NodeTermIdent* tid = std::get<NodeTermIdent*>(id->var);
					Token ident = tid->ident;
					std::string field_name = ident.value.value();
					if(!otype.root().is_object) {
						gen.GeneratorError(base->def, "bellow `.` except expression of type any object\nNOTE: but got " + otype.to_string());
					}
					std::string struct_name = otype.root().getobjectname();
					std::optional<Struct> st = gen.struct_lookup(struct_name);
					size_t field_offset = 0;
					if(st.has_value()) {
						std::optional<Field> field = gen.field_lookup(st.value(), field_name);
						if(!field.has_value()) {
							gen.GeneratorError(base->def, "object of type `" + otype.to_string() + "` doesn`t have field `" + field_name + "`");
						}
						field_offset = field.value().nth;
					}
					if(!st.has_value()) {
						std::optional<Interface> inter = gen.inter_lookup(struct_name);
						if(inter.has_value()) {
							std::optional<Field> field = gen.field_lookup(inter.value(), field_name);
							if(!field.has_value()) {
								gen.GeneratorError(base->def, "object of type `" + otype.to_string() + "` doesn`t have field `" + field_name + "`");
							}
							field_offset = field.value().nth;
						}
						else {
							assert(false && "unreacheable");
						}
					}
					gen.gen_expr(dot->lhs);
					if(lvalue) {
						gen.m_output << "    pop ecx\n";
						if(field_offset != 0U) {
							gen.m_output << "    add ecx, " << field_offset * 4U << "\n";
						}
						gen.m_output << "    push ecx\n";
					} else {
						gen.m_output << "    pop ecx\n";
						if(field_offset != 0U) {
							gen.m_output << "    push dword [ecx+" << field_offset * 4U << "]\n";
						} else {
						   gen.m_output << "    push dword [ecx]\n"; 
						}
					}
				} else {
					gen.GeneratorError(base->def, "after `.` except identificator");
				}
			}

			void operator()(const NodeBinExprArgs* args) const
			{
				for(int i = static_cast<int>(args->args.size()) - 1;i > -1;--i) {
					gen.gen_expr(args->args[i]);
				}
			}
		};
		
		BinExprVisitor visitor { .gen = *this , .lvalue = lvalue , .base = bin_expr };
		std::visit(visitor, bin_expr->var);
	}

	void gen_expr(const NodeExpr* expr, bool lvalue = false)
	{
		struct ExprVisitor {
			Generator& gen;
			bool lvalue;

			void operator()(const NodeTerm* term) const
			{
				gen.gen_term(term, lvalue);
			}

			void operator()(const NodeBinExpr* bin_expr) const
			{
				gen.gen_bin_expr(bin_expr, lvalue);
			}
		};

		ExprVisitor visitor { .gen = *this , .lvalue = lvalue };
		std::visit(visitor, expr->var);
	}

	size_t collect_alligns(const NodeScope* scope) noexcept {
		size_t fsz = 0ULL;
		for (const NodeStmt* stmt : scope->stmts) {
			if(std::holds_alternative<NodeStmtLet*>(stmt->var)) {
				fsz += 1;
			}
			else if(std::holds_alternative<NodeStmtLetNoAssign*>(stmt->var)) {
				fsz += 1;
			}
			else if(std::holds_alternative<NodeStmtCompileTimeIf*>(stmt->var)) {
				NodeStmtCompileTimeIf* ctif = std::get<NodeStmtCompileTimeIf*>(stmt->var);
				fsz += collect_alligns(ctif->_if);
				if(ctif->_else.has_value()) {
					fsz += collect_alligns(ctif->_else.value());
				}
			}
			else if(std::holds_alternative<NodeStmtBuffer*>(stmt->var)) {
				fsz += eval(std::get<NodeStmtBuffer*>(stmt->var)->size, std::get<NodeStmtBuffer*>(stmt->var)->def) / 4ULL;
			}
		}
		return fsz;
	}

	void gen_scope(const NodeScope* scope, size_t psize = 0, bool wbreak = false)
	{
		size_t sz = psize + collect_alligns(scope);
		if(wbreak) {
			m_break_scopes.push_back(sz);
		}
		begin_scope(sz);
		for (const NodeStmt* stmt : scope->stmts) {
			gen_stmt(stmt);
		}
		end_scope();
	}

	/*generate scope start procedure*/
	void gen_scope_sp(const NodeScope* scope, const NodeStmtProc* proc, const Procedure& __proc)
	{
		begin_scope(proc->params.size() + collect_alligns(scope));
		if(std::find(proc->attrs.begin(), proc->attrs.end(), ProcAttr::nostdargs) == proc->attrs.end()) {
			for(int i = 0;i < static_cast<int>(proc->params.size());++i) {
				create_var_va(__proc.params[i].first, __proc.params[i].second, proc->def);
				m_output << "    mov edx, dword [ebp+" << i * 4 + 8 << "]\n";
				m_output << "    mov dword [ebp-" << i * 4 + 4 << "], edx\n";
			}
		}
		for (const NodeStmt* stmt : scope->stmts) {
			gen_stmt(stmt);
		}
		end_scope_sp(m_cur_proc.value(), proc->def);
	}

	void convert(NodeExpr* expr, const DataType& to, const DataType& from, const Token& def) {
		if(to.root().is_object) {
			std::string objName = to.root().getobjectname();
			std::optional<Namespace*> _nms = namespace_lookup(objName);
			if(!_nms.has_value()) goto CONVERT_FAILED;
			NodeTermNmCall call;
			call.def = def;
			call.nm = objName;
			call.name = "new";
			std::vector<NodeExpr*> args;
			args.push_back(expr);
			NodeBinExprArgs bargs { .args = args};
			NodeBinExpr bexpr { .def = def , .var = &bargs };
			NodeExpr eargs{ .var = &bexpr };
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
		if(m_scopes_vi.size() == 0ULL) {
			GeneratorError(where, "can't create global variable with assignment");
		}
		std::optional<Var> ivar = var_lookup_cs(name);
		if(ivar.has_value()) {
			GeneratorError(where, "name `" + name + "` already in use");
		}
		DataType rtype;
		DataType got_t = type_of_expr(value);
		bool lval = false;
		if(!vtype.has_value()) rtype = got_t;
		else {
			DataType got_t = type_of_expr(value);
			rtype = vtype.value();
			substitute_template(rtype);
			if(rtype != got_t && !rtype.root().is_object) {
				if(rtype.root().link && !got_t.root().link) {
					lval = true;
				}
				else GeneratorError(where, "let except type " + rtype.to_string() + ", but got " + got_t.to_string() + ".");
			} else if(rtype != got_t && rtype.root().is_object) {
				convert(value, rtype, got_t, where);
				goto AFTER_GEN;
			}
		}
		if(got_t.root().rvalue) GeneratorError(where, "assigning rvalue-reference to variable.");
		gen_expr(value, lval);
AFTER_GEN:
		last_scope()[name] = { .name = name, .stack_loc = ++m_var_index * 4 , .type = rtype };
		m_output << "    pop ecx\n";
		m_output << "    mov dword [ebp-" << m_var_index * 4 << "], ecx\n";
	}

	void create_var_va(__str_ref name, const DataType& type, const Token& where) {
		if(m_scopes_vi.size() == 0ULL) {
			std::optional<GVar> ivar = gvar_lookup(name);
			if(ivar.has_value()) {
				GeneratorError(where, "name `" + name + "` already in use");
			}
			m_global_vars[name] = { .name = name, .type = type };
			return;
		}
		std::optional<Var> ivar = var_lookup_cs(name);
		if(ivar.has_value()) {
			GeneratorError(where, "name `" + name + "` already in use");
		}
		last_scope()[name] = { .name = name, .stack_loc = ++m_var_index * 4 , .type = type };
	}

	void create_var_va_wid(__str_ref name, const DataType& type, const Token& where) {
		std::optional<Var> ivar = var_lookup_cs(name);
		if(ivar.has_value()) {
			GeneratorError(where, "name `" + name + "` already in use");
		}
		last_scope()[name] = { .name = name, .stack_loc = m_var_index * 4 , .type = type };
	}

	void gen_if_pred(const NodeIfPred* pred, __str_ref end_label)
	{
		struct PredVisitor {
			Generator& gen;
			__str_ref end_label;

			void operator()(const NodeIfPredElif* elif) const
			{
				gen.gen_expr(elif->expr);
				gen.pop("eax");
				const std::string label = gen.create_label();
				gen.m_output << "    test eax, eax\n";
				gen.m_output << "    jz " << label << "\n";
				gen.gen_scope(elif->scope);
				gen.m_output << "    jmp " << end_label << "\n";
				gen.m_output << "    " << label << ":\n";
				if (elif->pred.has_value()) {
					gen.gen_if_pred(elif->pred.value(), end_label);
				}
			}

			void operator()(const NodeIfPredElse* else_) const
			{
				gen.gen_scope(else_->scope);
			}
		};

		PredVisitor visitor { .gen = *this, .end_label = end_label };
		std::visit(visitor, pred->var);
	}

	Procedure __proc_get(__str_ref name, const Token& def) {
		std::optional<Procedure> proc = proc_lookup(name);
		if(!proc.has_value()) {
			GeneratorError(def, "unkown procedure `" + name + "`");
		}
		return proc.value();
	}

	void __typecheck_call(const std::vector<NodeExpr*>& args, const std::vector<std::pair<std::string, DataType>>& params, const Token& def, const Procedure& proc, size_t* stack_allign) {
		if(params.size() == 0) {
			GeneratorError(def, "procedure `" + proc.name + "` don't excepts any arguments");
		}
		bool nosizedargs = std::find(proc.attrs.begin(), proc.attrs.end(), ProcAttr::nosizedargs) != proc.attrs.end();
		if(args.size() != proc.params.size() && !nosizedargs) {
			GeneratorError(def, "procedure `" + proc.name + "` excepts " + std::to_string(proc.params.size()) + " arguments\nNOTE: but got " + std::to_string(args.size()));
		}
		if(args.size() < proc.params.size() && nosizedargs) {
			GeneratorError(def, "procedure `" + proc.name + "` excepts minimum" + std::to_string(proc.params.size()) + " arguments\nNOTE: but got " + std::to_string(args.size()));
		}
		for(int i = 0;i < static_cast<int>(proc.params.size());++i) {
			DataType argtype = type_of_expr(args[i]);
			const DataType& ex_type = proc.params[i].second;
			TreeNode<BaseDataType>* current1 = argtype.list.get_root()->right;
			TreeNode<BaseDataType>* current2 = ex_type.list.get_root()->right;
			if(!argtype.root().arg_eq(ex_type.root())) {
				goto TYPECHECK_FAIL;
			}
			if(argtype.list.get_root()->size() != ex_type.list.get_root()->size()) {
				goto TYPECHECK_FAIL;
			}
			while(current1 != nullptr) {
				if(!current1->data.arg_eq(current2->data)) {
					goto TYPECHECK_FAIL;
				}
				current1 = current1->right;
				current2 = current2->right;
			}
			continue;
			TYPECHECK_FAIL:
			GeneratorError(def, "procedure `" + proc.name + "`\nexcept type " + proc.params[i].second.to_string() + " at " + std::to_string(i) + " argument\nNOTE: but found type " + type_of_expr(args[i]).to_string());
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
    	    bool has_template_args   = (root_node->right != nullptr);
    	    bool mapped_has_children = (mapped.list.get_root() &&
    	                                mapped.list.get_root()->right != nullptr);
	
    	    if (has_template_args && !mapped_has_children) {
    	        // Случай CT<T>: заменяем только имя типа,
    	        // оставляя цепочку параметров (<T, ...>) как есть.
    	        BaseDataType new_root = mapped.root();
    	        new_root.ptrlvl = old.ptrlvl;
    	        new_root.link   = old.link;
    	        new_root.rvalue = old.rvalue;
    	        root_node->data = new_root;
    	    } else {
    	        // Обычный T: заменяем весь DataType целиком.
    	        type = mapped;
    	        type.root().ptrlvl += old.ptrlvl;
    	        if (old.link)   type.root().link   = true;
    	        if (old.rvalue) type.root().rvalue = true;
    	    }
    	}
	
    	// Рекурсивно подставляем в аргументах шаблона (цепочка справа)
    	TreeNode<BaseDataType>* cur = type.list.get_root()->right;
		while (cur != nullptr) {
		    DataType tmp = cur->data;
		    substitute_template(tmp);           // здесь tmp может превратиться, например, в vector<int>
		
		    // 1) заменяем сам узел
		    cur->data = tmp.root();
		
		    // 2) если у tmp есть хвост (например, <int>), пришиваем его сюда
		    TreeNode<BaseDataType>* extra = nullptr;
		    TreeNode<BaseDataType>* tmp_root = tmp.list.get_root();
		    if (tmp_root) {
		        extra = tmp_root->right;        // это цепочка параметров шаблона: T1, T2, ...
		    }
		
		    TreeNode<BaseDataType>* last = cur;
		    while (extra != nullptr) {
		        // создаем новый узел после last и вешаем на него extra->data
		        auto* new_node = new TreeNode<BaseDataType>(extra->data);
		        new_node->right = last->right;  // подвешиваем старый хвост после нового узла
		        last->right = new_node;
		        last = new_node;
		        extra = extra->right;
		    }
		
		    // 3) переходим к узлу, который был исходным "следующим" после cur
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
    	    bool has_template_args   = (root_node->right != nullptr);
    	    bool mapped_has_children = (mapped.list.get_root() &&
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
		    if (tmp_root) {
		        extra = tmp_root->right;
		    }
		
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
		bool nosizedargs = std::find(proc.attrs.begin(), proc.attrs.end(), ProcAttr::nosizedargs) != proc.attrs.end();
		if(args.size() != proc.params.size() && !nosizedargs) {
			return false;
		}
		if(args.size() < proc.params.size() && nosizedargs) {
			return false;
		}
		for(int i = 0;i < static_cast<int>(proc.params.size());++i) {
			DataType argtype = type_of_expr(args[i]);
			const DataType& ex_type = proc.params[i].second;
			TreeNode<BaseDataType>* current1 = argtype.list.get_root()->right;
			TreeNode<BaseDataType>* current2 = ex_type.list.get_root()->right;
			if(!argtype.root().arg_eq(ex_type.root())) goto TRY_TYPECHECK_FAIL;
			if(argtype.list.get_root()->size() != ex_type.list.get_root()->size()) {
				goto TRY_TYPECHECK_FAIL;
			}
			while(current1 != nullptr) {
				if(current1->data != current2->data) goto TRY_TYPECHECK_FAIL;
				current1 = current1->right;
				current2 = current2->right;
			}
			continue;
			TRY_TYPECHECK_FAIL:
			return false;
		}
		return true;
	}

	std::vector<NodeExpr*> __getargs(NodeExpr* __expr) {
		return std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(__expr->var)->var)->args;
	}

	std::optional<int> __eval_ctcall(const NodeTermCall* call, const Token& def) {
		__str_ref name = call->name;
		if(name == "is_same_t") {
			if(!call->args.has_value()) {
				GeneratorError(def, "is_same_t excepts 2 args");
			}
			std::vector<NodeExpr*> args = __getargs(call->args.value());
			if(args.size() != 2) {
				GeneratorError(def, "is_same_t excepts 2 args");
			}
			std::optional<NodeTermType*> type_1 = ptools::get::type(args[0]);
			std::optional<NodeTermType*> type_2 = ptools::get::type(args[1]);
			DataType one;
			DataType two;
			if(type_1.has_value()) {
				one = type_1.value()->type;
			} else {
				one = type_of_expr(args[0]);
			}
			if(type_2.has_value()) {
				two = type_2.value()->type;
			} else {
				two = type_of_expr(args[1]);
			}
			substitute_template(one);
			substitute_template(two);
			return static_cast<int>(one == two);
		}
		if(name == "is_object_t") {
			if(!call->args.has_value()) {
				GeneratorError(def, "is_object_t excepts 1 args");
			}
			std::vector<NodeExpr*> args = __getargs(call->args.value());
			if(args.size() != 1) {
				GeneratorError(def, "is_object_t excepts 1 args");
			}
			std::optional<NodeTermType*> type = ptools::get::type(args[0]);
			DataType tp;
			if(type.has_value()) {
				tp = type.value()->type;
			} else {
				tp = type_of_expr(args[0]);
			}
			substitute_template(tp);
			return static_cast<int>(tp.root().is_object);
		}
		if(name == "ct_not") {
			if(!call->args.has_value()) {
				GeneratorError(def, "ct_not excepts 1 args");
			}
			std::vector<NodeExpr*> args = __getargs(call->args.value());
			if(args.size() != 1) {
				GeneratorError(def, "ct_not excepts 1 args");
			}
			return static_cast<int>(!(static_cast<bool>(eval(args[0], def))));
		}
		return std::nullopt;
	}

	int eval(const NodeExpr* expr, const Token& def) {
		int result = 0;
		if(std::holds_alternative<NodeTerm*>(expr->var)) {
			NodeTerm* nterm = std::get<NodeTerm*>(expr->var);
			if(std::holds_alternative<NodeTermIntLit*>(nterm->var)) {
				return std::stoul(std::get<NodeTermIntLit*>(std::get<NodeTerm*>(expr->var)->var)->int_lit.value.value());
			}
			if(std::holds_alternative<NodeTermCtMdefined*>(nterm->var)) {
				return static_cast<int>(std::get<NodeTermCtMdefined*>(std::get<NodeTerm*>(expr->var)->var)->value);
			}
			if(std::holds_alternative<NodeTermIdent*>(nterm->var)) {
				std::string cname = std::get<NodeTermIdent*>(nterm->var)->ident.value.value();
				if(cname == "iota") return CTX_IOTA++;
				if(cname == "reset") {
					int old = CTX_IOTA;
					CTX_IOTA = 0;
					return old;
				}
				if(cname == "true") return 1;
				if(cname == "false") return 0;
				std::optional<Constant> cns = const_lookup(cname);
				if(cns.has_value()) return cns.value().value;
			}
			if(std::holds_alternative<NodeTermCall*>(nterm->var)) {
				NodeTermCall* call = std::get<NodeTermCall*>(nterm->var);
				if(auto vl = __eval_ctcall(call, def)) return vl.value();
				GeneratorError(def, "unkown compile-time procedure `" + call->name + "`");
			}
		}
		if(std::holds_alternative<NodeBinExpr*>(expr->var)) {
			NodeBinExpr* nbin = std::get<NodeBinExpr*>(expr->var);
			if(std::holds_alternative<NodeBinExprAdd*>(nbin->var)) {
				NodeBinExprAdd* nadd = std::get<NodeBinExprAdd*>(nbin->var);
				return eval(nadd->lhs, def) + eval(nadd->rhs, def);
			}
			if(std::holds_alternative<NodeBinExprSub*>(nbin->var)) {
				NodeBinExprSub* nsub = std::get<NodeBinExprSub*>(nbin->var);
				return eval(nsub->lhs, def) - eval(nsub->rhs, def);
			}
			if(std::holds_alternative<NodeBinExprMulti*>(nbin->var)) {
				NodeBinExprMulti* nmul = std::get<NodeBinExprMulti*>(nbin->var);
				return eval(nmul->lhs, def) * eval(nmul->rhs, def);
			}
			if(std::holds_alternative<NodeBinExprDiv*>(nbin->var)) {
				NodeBinExprDiv* ndiv = std::get<NodeBinExprDiv*>(nbin->var);
				return eval(ndiv->lhs, def) / eval(ndiv->rhs, def);
			}
			if(std::holds_alternative<NodeBinExprEqEq*>(nbin->var)) {
				NodeBinExprEqEq* neqeq = std::get<NodeBinExprEqEq*>(nbin->var);
				return eval(neqeq->lhs, def) == eval(neqeq->rhs, def);
			}
			if(std::holds_alternative<NodeBinExprNotEq*>(nbin->var)) {
				NodeBinExprNotEq* nnoteq = std::get<NodeBinExprNotEq*>(nbin->var);
				return eval(nnoteq->lhs, def) != eval(nnoteq->rhs, def);
			}
			if(std::holds_alternative<NodeBinExprLess*>(nbin->var)) {
				NodeBinExprLess* nless = std::get<NodeBinExprLess*>(nbin->var);
				return eval(nless->lhs, def) < eval(nless->rhs, def);
			}
			if(std::holds_alternative<NodeBinExprAbove*>(nbin->var)) {
				NodeBinExprAbove* nabove = std::get<NodeBinExprAbove*>(nbin->var);
				return eval(nabove->lhs, def) > eval(nabove->rhs, def);
			}
			if(std::holds_alternative<NodeBinExprAnd*>(nbin->var)) {
				NodeBinExprAnd* nand = std::get<NodeBinExprAnd*>(nbin->var);
				return eval(nand->lhs, def) && eval(nand->rhs, def);
			}
			if(std::holds_alternative<NodeBinExprOr*>(nbin->var)) {
				NodeBinExprOr* nor = std::get<NodeBinExprOr*>(nbin->var);
				return eval(nor->lhs, def) || eval(nor->rhs, def);
			}
		}
		return result;
	}

	bool in_namespace() {
		return m_cur_namespace != NULL;
	}

	__map<std::string, DataType> try_derive_templates(std::vector<DataType>& targs, const std::vector<std::pair<std::string, DataType>>& params, const Token& def, __stdvec<std::string>* templates, const std::vector<NodeExpr*> args, const Procedure& proc) {
		if(params.empty()) GeneratorError(def, "can't substitute type " + *templates->begin() + ", params empty.");
		if(params.size() != args.size()) GeneratorError(def, "procedure `" + proc.name = "` excepts " + std::to_string(params.size()) + " args, but got " + std::to_string(args.size()) + ".");
		assert(templates != NULL);
		assert(!templates->empty());
		targs.reserve(templates->size());
		for(int i = 0;i < static_cast<int>(templates->size());++i) {
			targs.emplace_back(SimpleDataType::_void);
		}
		size_t redo_count {0};
	REDO_DERIVE:
		for(int i = 0;i < static_cast<int>(params.size());++i) {
			int counter {-1};
			int temp_s {-1};
			bool is_temp_s = false;
			for(int j = 0;j < static_cast<int>(templates->size());++j) {
				if(!params[i].second.is_object()) continue;
				if(templates->operator[](j) == params[i].second.getobjectname()) {
					counter = j;
					break;
				}
				TreeNode<BaseDataType>* current = params[i].second.list.get_root()->right;
				temp_s = 0;
				while(current != nullptr) {
					if(current->data.is_object && templates->operator[](j) == current->data.getobjectname()) {
						if(targs[j].is_simple() && targs[j].getsimpletype() == SimpleDataType::_void) {
							counter = j;
							is_temp_s = true;
							break;
						}
					}
					temp_s++;
					current = current->right;
				}
				if(is_temp_s) break;
				else temp_s = -1;
			}
			if (counter != -1) {
    			if (targs[counter].is_simple() && targs[counter].getsimpletype() == SimpleDataType::_void) {
    			    if (is_temp_s) {
    			        DataType ct = type_of_expr(args[i]);
    			        TreeNode<BaseDataType>* cur = ct.list.get_root()->right;
			
    			        // Аккуратно двигаемся по цепочке, учитывая, что у аргумента
    			        // цепочка может быть короче, чем у параметра.
    			        for (int k = 0; k < temp_s && cur; ++k) {
    			            cur = cur->right;
    			        }
			
    			        if (cur) {
    			            // Удалось сопоставить форму параметра и аргумента —
    			            // забираем поддерево типов начиная с cur.
    			            targs[counter] = create_datatype_from_chain(cur);
    			        }
    			        // else: форма аргумента не подходит под параметр — просто не
    			        // выводим этот шаблонный параметр отсюда, пусть попробуют
    			        // вывести его по другим параметрам.
    			    }
    			    else {
    					DataType arg_tp = type_of_expr(args[i]);
    					// снимаем ссылку и rvalue-ссылку
    					arg_tp.root().link = false;
    					arg_tp.root().rvalue = false;
    					targs[counter] = arg_tp;
					}
    			}
    			counter = -1;
    			is_temp_s = false;
    			temp_s = -1;
			}
		}
		size_t i {0};
		__map<std::string, DataType> temps;
		size_t counter {0};
		for(auto&& el : *templates) {
			if(targs[i].root().is_simple() && targs[i].root().getsimpletype() == SimpleDataType::_void) {
				if(redo_count > 1)
					GeneratorError(def, "failed to substitute template argument `" + templates->operator[](i) + "`.");
				else {
					redo_count++;
					goto REDO_DERIVE;
				}
			}
			i++;
			temps[el] = targs[counter++];
		}
		return temps;
	}

	std::pair<__map<std::string, DataType>, bool> try_derive_templates_no_err(std::vector<DataType> targs, const std::vector<std::pair<std::string, DataType>> params, const Token& def, __stdvec<std::string>* templates, const std::vector<NodeExpr*> args, const Procedure& proc) {
		using __L_map = __map<std::string, DataType>;
		if(params.empty()) return std::make_pair(__L_map{}, false);
		if(params.size() != args.size()) GeneratorError(def, "procedure `" + proc.name = "` excepts " + std::to_string(params.size()) + " args, but got " + std::to_string(args.size()) + ".");
		assert(templates != NULL);
		assert(!templates->empty());
		targs.reserve(templates->size());
		for(int i = 0;i < static_cast<int>(templates->size());++i) {
			targs.emplace_back(SimpleDataType::_void);
		}
		size_t redo_count {0};
	REDO_DERIVE:
		for(int i = 0;i < static_cast<int>(params.size());++i) {
			int counter {-1};
			int temp_s {-1};
			bool is_temp_s = false;
			for(int j = 0;j < static_cast<int>(templates->size());++j) {
				if(!params[i].second.is_object()) continue;
				if(templates->operator[](j) == params[i].second.getobjectname()) {
					counter = j;
					break;
				}
				TreeNode<BaseDataType>* current = params[i].second.list.get_root()->right;
				temp_s = 0;
				while(current != nullptr) {
					if(current->data.is_object && templates->operator[](j) == current->data.getobjectname()) {
						if(targs[j].is_simple() && targs[j].getsimpletype() == SimpleDataType::_void) {
							counter = j;
							is_temp_s = true;
							break;
						}
					}
					temp_s++;
					current = current->right;
				}
				if(is_temp_s) break;
				else temp_s = -1;
			}
			if (counter != -1) {
			    if (targs[counter].is_simple() && targs[counter].getsimpletype() == SimpleDataType::_void) {
			        if (is_temp_s) {
			            DataType ct = type_of_expr(args[i]);
			            TreeNode<BaseDataType>* cur = ct.list.get_root()->right;
			
			            for (int k = 0; k < temp_s && cur; ++k) {
			                cur = cur->right;
			            }
			
			            if (!cur) {
			                // Форма аргумента не соответствует форме параметра
			                // (например, параметр deque<T>, а аргумент int) —
			                // вывод шаблонных аргументов для этого кандидата невозможен.
			                return std::make_pair(__L_map{}, false);
			            }
			
			            targs[counter] = create_datatype_from_chain(cur);
			        }
			        else {
			            DataType arg_tp = type_of_expr(args[i]);
    					arg_tp.root().link = false;
    					arg_tp.root().rvalue = false;
    					targs[counter] = arg_tp;
			        }
			    }
			    counter = -1;
			    is_temp_s = false;
			    temp_s = -1;
			}
		}
		size_t i {0};
		__map<std::string, DataType> temps;
		size_t counter {0};
		for(auto&& el : *templates) {
			if(targs[i].root().is_simple() && targs[i].root().getsimpletype() == SimpleDataType::_void) {
				if(redo_count > 1)
					return std::make_pair(__L_map{}, false);
				else {
					redo_count++;
					goto REDO_DERIVE;
				}
			}
			i++;
			temps[el] = targs[counter++];
		}
		return std::make_pair(temps, true);
	}

	void resolve_overrides(Procedure* proc, std::optional<NodeExpr*> args, const Token& def) {
		assert(proc != NULL);
		Procedure* cp = NULL;
		if(!proc->overrides.empty()) {
			if(!args.has_value()) {
				if(!proc->params.empty() && proc->overrides.size() == 0) GeneratorError(def, "procedure `" + proc->name + "` excepts " + std::to_string(proc->params.size()) + " arguments, but got 0.");
				if(proc->params.empty()) return;
				for(int i = 0;i < static_cast<int>(proc->overrides.size());++i) {
					cp = proc->overrides[i];
					if(cp->params.size() == 0ULL) {
						*proc = *cp;
						return;
					}
				}
			}
			else {
				for(int i = 0;i < static_cast<int>(proc->overrides.size());++i) {
					cp = proc->overrides[i];
					if(__try_typecheck_call(__getargs(args.value()), *cp)) {
						*proc = *cp;
						return;
					}
				}
			}
		}
		if(!proc->overrides.empty()) {
			if(!__try_typecheck_call(__getargs(args.value()), *proc)) {
				std::vector<NodeExpr*> args_v = __getargs(args.value());
				std::string args_s;
				for(int i = 0;i < static_cast<int>(args_v.size());++i) {
					std::string tmp(type_of_expr(args_v[i]).to_string());
					args_s += tmp.substr(1, tmp.size() - 2);
					if(i != static_cast<int>(args_v.size()) - 1) {
						args_s += ", ";
					}
				}
				DiagnosticMessage(def, "error", "no match candidate for call procedure " + proc->name + "(" + args_s + ").", 0);
				std::string args_s1;
				for(int i = 0;i < static_cast<int>(proc->params.size());++i) {
					std::string tmp(proc->params[i].second.to_string());
					args_s1 += tmp.substr(1, tmp.size() - 2);
					if(i != static_cast<int>(proc->params.size()) - 1) {
						args_s1 += ", ";
					}
				}
				DiagnosticMessage(proc->def, "note", "candidate " + proc->name + "(" + args_s1 + ").", 0);
				for(int i = 0;i < static_cast<int>(proc->overrides.size());++i) {
					Procedure* cp = proc->overrides[i];
					std::string args_s;
					for(int i = 0;i < static_cast<int>(cp->params.size());++i) {
						std::string tmp(cp->params[i].second.to_string());
						args_s += tmp.substr(1, tmp.size() - 2);
						if(i != static_cast<int>(cp->params.size()) - 1) {
							args_s += ", ";
						}
					}
					DiagnosticMessage(cp->def, "note", "candidate " + proc->name + "(" + args_s + ").", 0);
				}
				exit(1);
			}
		}
	}

	__stdvec<Procedure> collect_candidates_earg(Procedure* proc) {
		__stdvec<Procedure> res;
		if(proc->params.empty()) res.push_back(*proc);
		for(int i = 0;i < static_cast<int>(proc->overrides.size());++i) {
			if(proc->overrides[i]->params.empty()) res.push_back(*proc->overrides[i]);
		}
		return res;
	}

	__stdvec<Procedure> collect_candidates(Procedure* proc, size_t args_size) {
		__stdvec<Procedure> res;
		if(proc->params.size() == args_size) res.push_back(*proc);
		for(int i = 0;i < static_cast<int>(proc->overrides.size());++i) {
			if(proc->overrides[i]->params.size() == args_size) res.push_back(*proc->overrides[i]);
		}
		return res;
	}

	void resolve_overrides_tp(Procedure* proc, std::optional<NodeExpr*> args, const Token& def, __stdvec<DataType> targs) {
		assert(proc != NULL);
		Procedure copy_of_proc = *proc;
		Procedure* ptr_to_proc = proc;
		__stdvec<Procedure> candidates;
		if(args.has_value()) candidates = collect_candidates(ptr_to_proc, __getargs(args.value()).size());
		else {
			candidates = collect_candidates_earg(ptr_to_proc);
			for(int i = 0;i < static_cast<int>(candidates.size());++i) {
				if(candidates[i].params.empty()) {
					*ptr_to_proc = candidates[i];
					return;
				}
			}
		}
		for(int i = 0;i < static_cast<int>(candidates.size());++i) {
			Procedure cur_c = candidates[i];
			__map<std::string, DataType> temps;
			if(cur_c.templates != NULL && targs.empty()) {
				auto res = try_derive_templates_no_err(targs, cur_c.params, def, cur_c.templates, __getargs(args.value()), cur_c);
				if(!res.second) {
					continue;
				}
				else temps = std::move(res.first);
			} else if(cur_c.templates != NULL && cur_c.templates->size() != targs.size()) {
				continue;
			} else if(cur_c.templates != NULL && targs.size() == cur_c.templates->size()) {
				size_t counter {0};
				for(auto&& el : *cur_c.templates) {
					temps[el] = targs[counter];
					counter++;
				}
			}
			if(cur_c.templates != NULL) {
				substitute_template_params(temps, cur_c.params);
			}
			if(__try_typecheck_call(__getargs(args.value()), cur_c)) {
				*ptr_to_proc = candidates[i];
				return;
			}
		}
		std::vector<NodeExpr*> args_v = __getargs(args.value());
		std::string args_s;
		for(int i = 0;i < static_cast<int>(args_v.size());++i) {
			args_s += type_of_expr(args_v[i]).to_string();
			if(i != static_cast<int>(args_v.size()) - 1) {
				args_s += ", ";
			}
		}
		DiagnosticMessage(def, "error", "no match candidate for call procedure " + proc->name + "(" + args_s + ").", 0);
		std::string args_s1;
		for(int i = 0;i < static_cast<int>(proc->params.size());++i) {
			args_s1 += proc->params[i].second.to_string();
			if(i != static_cast<int>(proc->params.size()) - 1) {
				args_s1 += ", ";
			}
		}
		DiagnosticMessage(proc->def, "note", "candidate " + proc->name + "(" + args_s1 + ").", 0);
		for(int i = 0;i < static_cast<int>(proc->overrides.size());++i) {
			Procedure* cp = proc->overrides[i];
			std::string args_s;
			for(int i = 0;i < static_cast<int>(cp->params.size());++i) {
				args_s += cp->params[i].second.to_string();
				if(i != static_cast<int>(cp->params.size()) - 1) {
					args_s += ", ";
				}
			}
			DiagnosticMessage(cp->def, "note", "candidate " + proc->name + "(" + args_s + ").", 0);
		}
		exit(1);
	}

	void gen_args(const __stdvec<NodeExpr*>& args, __stdvec<std::pair<std::string, DataType>>& params) {
		for(int i = static_cast<int>(args.size()) - 1;i > -1;--i) {
			if(i < static_cast<int>(params.size())) {
				DataType tp = type_of_expr(args[i]);
				if(tp.root().link) gen_expr(args[i], false);
				else gen_expr(args[i], params[i].second.root().link);
			} else gen_expr(args[i], false);
		}
	}

	void substitute_template_params(__map<std::string, DataType>& temps, __stdvec<std::pair<std::string, DataType>>& params) {
		for(int i = 0;i < static_cast<int>(params.size());++i) {
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
				if(etype != BaseDataTypeInt) {
					gen.GeneratorError(stmt_exit->def, "`exit` except type `int`\nNOTE: but got type " + etype.to_string());
				}
				gen.gen_expr(stmt_exit->expr);
				gen.m_output << "    call ExitProcess@4\n";
			}

			void operator()(const NodeStmtProc* stmt_proc)
			{
				std::optional<Procedure> proc = gen.proc_lookup(stmt_proc->name);
				bool override = false;
				Procedure* movs = NULL;
				if(proc.has_value() && !proc.value().prototype && !gen.in_namespace()) {
					if(stmt_proc->params != proc.value().params) {
						Procedure* ovs = gen.m_allocator.emplace<Procedure>();
						ovs->name = stmt_proc->name;
						ovs->params = stmt_proc->params;
						ovs->rettype = stmt_proc->rettype;
						ovs->stack_allign = stmt_proc->params.size() + gen.collect_alligns(stmt_proc->scope);
						ovs->attrs = stmt_proc->attrs;
						ovs->def = stmt_proc->def;
						ovs->prototype = stmt_proc->prototype;
						ovs->override = true;
						ovs->uniq_sign = std::nullopt;
						ovs->scope = stmt_proc->scope;
						ovs->from = stmt_proc;
						ovs->templates = stmt_proc->templates;
						ovs->overload_nth = proc.value().overrides.size() + 1;
						movs = ovs;
						gen.m_procs[stmt_proc->name].overrides.push_back(ovs);
						override = true;
					} else {
						Token pdef = proc.value().def;
						gen.DiagnosticMessage(stmt_proc->def, "error", "procedure `" + stmt_proc->name + "` redefenition.", 0);
						gen.DiagnosticMessage(pdef, "note", "first defenition here.", 0);
						exit(1);
					}
				}
				else if(proc.has_value() && proc.value().prototype && !gen.in_namespace()) {
					if(proc.value().params.size() != stmt_proc->params.size()) {
						gen.GeneratorError(stmt_proc->def, "prototype of procedure and definition have different params sizes.\nNOTE: except `" + std::to_string(proc.value().params.size()) + "` but got `" + std::to_string(stmt_proc->params.size()) + "`.");
					}
					if(proc.value().rettype != stmt_proc->rettype) {
						gen.GeneratorError(stmt_proc->def, "prototype of procedure and defenition have other return types.\nNOTE: prototype return type - " + proc.value().rettype.to_string() + "\nNOTE: defenition return type - " + stmt_proc->rettype.to_string());
					}
				}
				size_t fsz = 0;
				if(!stmt_proc->prototype && stmt_proc->rettype != BaseDataTypeConst) {
					fsz = gen.collect_alligns(stmt_proc->scope);
				}
				if(!override) {
					if(gen.in_namespace()) {
						const auto& search = gen.m_cur_namespace->procs.find(stmt_proc->name);
						if(search != gen.m_cur_namespace->procs.end()) {
							Procedure& npc = search->second;
							if(stmt_proc->params.size() == npc.params.size() && npc.params == stmt_proc->params) {
								Token pdef = npc.def;
								gen.DiagnosticMessage(stmt_proc->def, "error", "procedure `" + stmt_proc->name + "` redefenition.", 0);
								gen.DiagnosticMessage(pdef, "note", "first defenition here.", 0);
								exit(1);
							} else {
								Procedure* ovs = gen.m_allocator.emplace<Procedure>();
								ovs->name = stmt_proc->name;
								ovs->params = stmt_proc->params;
								ovs->rettype = stmt_proc->rettype;
								ovs->stack_allign = stmt_proc->params.size() + gen.collect_alligns(stmt_proc->scope);
								ovs->attrs = stmt_proc->attrs;
								ovs->def = stmt_proc->def;
								ovs->prototype = stmt_proc->prototype;
								ovs->override = true;
								ovs->uniq_sign = std::nullopt;
								ovs->scope = stmt_proc->scope;
								ovs->from = stmt_proc;
								ovs->templates = stmt_proc->templates;
								ovs->overload_nth = npc.overrides.size() + 1;
								movs = ovs;
								npc.overrides.push_back(ovs);
								override = true;
							}
						} else {
							gen.m_cur_namespace->procs[stmt_proc->name] = { .name = stmt_proc->name , .params = stmt_proc->params , .rettype = stmt_proc->rettype, .stack_allign = stmt_proc->params.size() + fsz, .attrs = stmt_proc->attrs , .def = stmt_proc->def, .prototype = stmt_proc->prototype, .overrides = {}, .override = false , .uniq_sign = std::nullopt, .templates = stmt_proc->templates , .scope = stmt_proc->scope , .from = stmt_proc , .instanceated = {} , .mbn = "", .overload_nth = 0 };
						}
						if(stmt_proc->prototype) return;
					} else {
						gen.m_procs[stmt_proc->name] = { .name = stmt_proc->name , .params = stmt_proc->params , .rettype = stmt_proc->rettype, .stack_allign = stmt_proc->params.size() + fsz, .attrs = stmt_proc->attrs , .def = stmt_proc->def, .prototype = stmt_proc->prototype, .overrides = {}, .override = false , .uniq_sign = std::nullopt , .templates = stmt_proc->templates , .scope = stmt_proc->scope , .from = stmt_proc , .instanceated = {} , .mbn = "" , .overload_nth = 0 };
					}
				}
				if(stmt_proc->rettype == BaseDataTypeConst) return;
				if(stmt_proc->templates != NULL) return;
				if(stmt_proc->prototype) return;
				if(override && movs->templates != NULL) return;
				std::vector<ProcAttr> attrs = stmt_proc->attrs; 
				bool noprolog = std::find(attrs.begin(), attrs.end(), ProcAttr::noprolog) != attrs.end();
				if(stmt_proc->name != "main") {
					if(gen.in_namespace()) gen.m_output << gen.m_cur_namespace->name << "@" << stmt_proc->name;
					else gen.m_output << stmt_proc->name;
					if(override) {
						gen.m_output << movs->get_sign();
					}
				} else {
					gen.m_output << "__main";
				}
				gen.m_output << ":\n";
				if(!noprolog) {
					gen.m_output << "    push ebp\n";
					gen.m_output << "    mov ebp, esp\n";
				}
				if(!override) {
					if(gen.in_namespace()) gen.m_cur_proc = gen.m_cur_namespace->procs[stmt_proc->name];
					else gen.m_cur_proc = gen.m_procs[stmt_proc->name];
				} else gen.m_cur_proc = *movs;
				if(!override) {
					if(gen.in_namespace()) gen.gen_traceback_push_nm(gen.m_cur_namespace->procs[stmt_proc->name], gen.m_cur_namespace->name);
					else gen.gen_traceback_push(gen.m_procs[stmt_proc->name]);
				} else gen.gen_traceback_push(*movs);
				if(stmt_proc->name == "main") {
					gen.m_output << "    call _BPM_init_\n";
				}
				gen.gen_scope_sp(stmt_proc->scope, stmt_proc, gen.m_cur_proc.value());
				gen.m_cur_proc = std::nullopt;
				if(stmt_proc->rettype.root() == BaseDataTypeVoid) {
					gen.m_output << "    call traceback_pop\n";
				} else {
					gen.m_output << "    push eax\n";
					gen.m_output << "    call traceback_pop\n";
					gen.m_output << "    pop eax\n";
				}
				if(stmt_proc->name == "main") {
					gen.m_output << "    xor eax, eax\n";
				}
				if(!noprolog) {
					gen.m_output << "    pop ebp\n";
				}
				gen.m_output << "    ret\n\n";
				gen.m_var_index = 0U;
			}

			void operator()(const NodeStmtReturn* stmt_return) const
			{
				std::optional<Procedure> cproc = gen.m_cur_proc;
				if(!cproc.has_value()) {
					gen.GeneratorError(stmt_return->def, "return without procedure");
				}
				DataType rettype = cproc.value().rettype;
				if(rettype.root() == BaseDataTypeVoid && stmt_return->expr.has_value()) {
					gen.GeneratorError(stmt_return->def, "return from void procedure with value");
				} else if(!stmt_return->expr.has_value() && rettype.root() != BaseDataTypeVoid) {
					gen.GeneratorError(stmt_return->def, "procedure `" + cproc.value().name + "` at return except type " + rettype.to_string() + "\nNOTE: but got empty return");
				}
				if(stmt_return->expr.has_value() && gen.type_of_expr(stmt_return->expr.value()) != rettype) {
					gen.GeneratorError(stmt_return->def, "procedure `" + cproc.value().name + "` at return except type " + rettype.to_string() + "\nNOTE: but got type " + gen.type_of_expr(stmt_return->expr.value()).to_string());
				}
				if(stmt_return->expr.has_value()) {
					if (std::holds_alternative<NodeTerm*>(stmt_return->expr.value()->var)) {
                    	NodeTerm* t = std::get<NodeTerm*>(stmt_return->expr.value()->var);
                    	if (std::holds_alternative<NodeTermAmpersand*>(t->var)) {
                        	NodeTermAmpersand* as_amp = std::get<NodeTermAmpersand*>(t->var);
                        	if (std::holds_alternative<NodeTerm*>(as_amp->expr->var)) {
                        		NodeTerm* tt = std::get<NodeTerm*>(as_amp->expr->var);
                        		if (std::holds_alternative<NodeTermIdent*>(tt->var)) {
                        			NodeTermIdent* id = std::get<NodeTermIdent*>(tt->var);
                        			std::string name = id->ident.value.value();
                        			if (gen.var_lookup_cs(name).has_value()) { // _cs ищет только в текущем скоупе? Или во всех локальных?
                            		 	// Если мы берем адрес локальной переменной...
                            		 	// ...и если этот код находится внутри выражения return (это сложно отследить здесь).
                            		 	// Но можно выдать предупреждение.
                            		 	gen.GeneratorWarning(stmt_return->def, "Taking address of local variable `" + name + "`. "
                            		                      "Ensure this pointer does not outlive the scope.");
                        			}
                        		}
                        	}
                    	}
                	}

					gen.gen_expr(stmt_return->expr.value());
					gen.pop("eax");
				}
				if(gen.in_namespace()) cproc.value().gen_ret(gen, gen.m_cur_namespace->name);
				else cproc.value().gen_ret(gen, std::nullopt);
			}

			void operator()(const NodeStmtLet* stmt_let) const
			{
				gen.create_var(stmt_let->ident.value.value(), stmt_let->expr, stmt_let->ident, stmt_let->type);
			}

			void operator()(const NodeStmtLetNoAssign* stmt_let) const
			{
				gen.create_var_va(stmt_let->ident.value.value(), stmt_let->type, stmt_let->ident);
			}

			void operator()(const NodeStmtCompileTimeIf* stmt_ctif) const {
				bool condition = gen.eval(stmt_ctif->condition, stmt_ctif->def);
				if(condition) {
					for(const auto stmt : stmt_ctif->_if->stmts) {
						gen.gen_stmt(stmt);
					}
				} else {
					if(stmt_ctif->_else.has_value()) {
						for(const auto stmt : stmt_ctif->_else.value()->stmts) {
							gen.gen_stmt(stmt);
						}
					}
				}
			}

			void operator()(const NodeStmtAssign* stmt_assign) const
			{
				NodeExpr* lvalue = stmt_assign->lvalue;
				DataType ltype = gen.type_of_expr(stmt_assign->lvalue);
				DataType vtype = gen.type_of_expr(stmt_assign->expr);
				if(ltype.root().link && vtype.root().link) gen.GeneratorError(stmt_assign->def, "reference variable reassigning.");
				if(!ltype.root().is_object) {
					if(ltype != vtype) {
						if(ltype.root().link && !vtype.root().link) {
							NodeTermUnref unr;
							unr.def = stmt_assign->def;
							unr.expr = lvalue;
							NodeTerm at;
							at.var = &unr;
							NodeExpr ae;
							ae.var = &at;

							NodeStmtAssign newas;
							newas.def = stmt_assign->def;
							newas.lvalue = &ae;
							newas.expr = stmt_assign->expr;
							NodeStmt stmt;
							stmt.var = &newas;
							gen.gen_stmt(&stmt);
							return;
						}
						else gen.GeneratorError(stmt_assign->def, "at = except type " + ltype.to_string() + "\nNOTE: but got " + vtype.to_string());
					}
					if(std::holds_alternative<NodeTerm*>(lvalue->var)) {
						NodeTerm* lvterm = std::get<NodeTerm*>(lvalue->var);
						if(std::holds_alternative<NodeTermIdent*>(lvterm->var)) {
							NodeTermIdent* lvident = std::get<NodeTermIdent*>(lvterm->var);
							std::string name = lvident->ident.value.value();
							std::optional<Var> var = gen.var_lookup(name);
							if(var.has_value()) {
								gen.gen_expr(stmt_assign->expr);
								gen.pop("edx");
								gen.m_output << "    mov dword [ebp-" << var.value().stack_loc << "], edx\n";
								return;
							}
							std::optional<GVar> ivar = gen.gvar_lookup(name);
							if(ivar.has_value()) {
								gen.gen_expr(stmt_assign->expr);
								gen.pop("edx");
								gen.m_output << "    mov dword [v_" << ivar.value().name << "], edx\n";
								return;
							}
							gen.GeneratorError(stmt_assign->def, "unkown variable `" + name + "` at assignment");
							return;
						}
					}
					gen.gen_expr(stmt_assign->lvalue, true);
					gen.gen_expr(stmt_assign->expr);
					gen.pop("ecx");
					gen.pop("edx");
					gen.m_output << "    mov dword [edx], ecx\n";
				}
				else {
					NodeStmtMtCall call;
					call.def = stmt_assign->def;
					call.mt = stmt_assign->lvalue;
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
				if(!oneT.root().is_object) {
					if(oneT != twoT && !(oneT.root() == BaseDataTypePtr && twoT.root() == BaseDataTypeInt)) {
						gen.GeneratorError(stmt_assign->def, "at += except type " + oneT.to_string() + "\nNOTE: but got " + twoT.to_string());
					}
					if(auto ident = ptools::get::ident(stmt_assign->lvalue)) {
						Var vr = gen.var_lookup_err(ident.value()->ident.value.value(), stmt_assign->def);
						gen.gen_expr(stmt_assign->expr);
						gen.asmg.pop("edx");
						gen.asmg.add(vr.ref(), "edx");
						return;
					}
					gen.gen_expr(stmt_assign->lvalue, true);
					gen.gen_expr(stmt_assign->expr);
					gen.pop("ecx");
					gen.pop("edx");
					gen.m_output << "    add dword [edx], ecx\n";
				}
				else {
					NodeStmtMtCall call;
					call.def = stmt_assign->def;
					call.mt = stmt_assign->lvalue;
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
				if(!oneT.root().is_object) {
					if(oneT != twoT && !(oneT.root() == BaseDataTypePtr && twoT.root() == BaseDataTypeInt)) {
						gen.GeneratorError(stmt_assign->def, "at -= except type " + oneT.to_string() + "\nNOTE: but got " + twoT.to_string());
					}
					if(auto ident = ptools::get::ident(stmt_assign->lvalue)) {
						Var vr = gen.var_lookup_err(ident.value()->ident.value.value(), stmt_assign->def);
						gen.gen_expr(stmt_assign->expr);
						gen.asmg.pop("edx");
						gen.asmg.sub(vr.ref(), "edx");
						return;
					}
					gen.gen_expr(stmt_assign->lvalue, true);
					gen.gen_expr(stmt_assign->expr);
					gen.pop("ecx");
					gen.pop("edx");
					gen.m_output << "    sub dword [edx], ecx\n";
				}
				else {
					NodeStmtMtCall call;
					call.def = stmt_assign->def;
					call.mt = stmt_assign->lvalue;
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
				gen.pop("ecx");
				gen.pop("edx");
				gen.m_output << "    mov eax, dword [edx]\n";
				gen.m_output << "    imul eax, ecx\n";
				gen.m_output << "    mov dword [edx], eax\n";
			}

			void operator()(const NodeStmtDivBy* stmt_assign) const
			{
				gen.gen_expr(stmt_assign->lvalue, true);
				gen.gen_expr(stmt_assign->expr);
				gen.pop("edi");
				gen.pop("esi");
				gen.m_output << "    xor edx, edx\n";
				gen.m_output << "    mov eax, dword [esi]\n";
				gen.m_output << "    div edi\n";
				gen.m_output << "    mov dword [esi], eax\n";
			}

			void operator()(NodeStmtCall* stmt_call) const
			{
				const std::string name = stmt_call->def.value.value();
				Procedure proc = gen.__proc_get(stmt_call->name, stmt_call->def);
				if(!proc.overrides.empty()) gen.resolve_overrides_tp(&proc, stmt_call->args, stmt_call->def, stmt_call->targs);
				__map<std::string, DataType> temps;
				bool substituted = false;
				if(proc.templates != NULL && stmt_call->targs.empty()) {
					if(stmt_call->args.has_value()) {
						temps = gen.try_derive_templates(stmt_call->targs, proc.params, stmt_call->def, proc.templates, gen.__getargs(stmt_call->args.value()), proc);
						substituted = true;
					}
				}
				if(proc.templates != NULL && stmt_call->targs.empty()) {
					gen.GeneratorError(stmt_call->def, "procedure `" + stmt_call->name + "` excepts template arguments in <...>.");
				}
				std::string tsign;
				if(!stmt_call->targs.empty()) {
					gen.substitute_template(proc.rettype);
					for(int i = 0;i < static_cast<int>(stmt_call->targs.size());++i) {
						DataType t = stmt_call->targs[i];   // копия
        				gen.substitute_template(t);
        				tsign += t.sign();
					}
					if(!proc.instanceated[tsign]) {
						Generator dop_gen(gen);
						gen.m_procs[stmt_call->name].instanceated[tsign] = true;
						dop_gen.m_output << proc.name << tsign;
						dop_gen.m_output << ":\n";
						dop_gen.m_output << "    push ebp\n";
						dop_gen.m_output << "    mov ebp, esp\n";
						dop_gen.gen_traceback_push(proc);
						dop_gen.m_tsigns.push_back(tsign);
						size_t counter {0};
						if(substituted) {
							dop_gen.m_temps.push_back(temps);
						}
						else {
							dop_gen.m_temps.emplace_back(__map<std::string, DataType>{});
							for(auto&& el : *proc.templates) {
								dop_gen.m_temps[dop_gen.m_temps.size() - 1][el] = stmt_call->targs[counter++];
							}
						}
						if(substituted) gen.substitute_template_params(temps, proc.params);
						else gen.substitute_template_params(dop_gen.m_temps.back(), proc.params);
						dop_gen.m_cur_proc = proc;
						dop_gen.gen_scope_sp(proc.scope, proc.from, proc);
						if(!substituted) {
							temps = std::move(dop_gen.m_temps.back());
						}
						gen.m_strings = std::move(dop_gen.m_strings);
						dop_gen.m_temps.pop_back();
						dop_gen.m_tsigns.pop_back();
						dop_gen.m_output << "    call traceback_pop\n";
						dop_gen.m_output << "    pop ebp\n";
						dop_gen.m_output << "    ret\n\n";
						(*gen.m_result) << (dop_gen.m_output.str());
					}
				}
				size_t stack_allign = 0;
				if(stmt_call->args.has_value()) {
					gen.__typecheck_call(gen.__getargs(stmt_call->args.value()), proc.params, stmt_call->def, proc, &stack_allign);
				} else if(proc.params.size() != 0ULL) {
					gen.GeneratorError(stmt_call->def, "procedure `" + proc.name + "` excepts " + std::to_string(proc.params.size()) + " arguments\nNOTE: but got 0");
				}
				if(stmt_call->args.has_value()) {
					gen.gen_args(gen.__getargs(stmt_call->args.value()), proc.params);
				}
				gen.m_output << "    call " << name;
				if(!stmt_call->targs.empty()) {
					gen.m_output << tsign;
				} else {
					if(proc.override) gen.m_output << proc.get_sign();
				}
				gen.m_output << "\n";
				if(stack_allign != 0) {
					gen.m_output << "    add esp, " << stack_allign * 4 << "\n";
				}
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
				gen.pop("eax");
				const std::string label = gen.create_label();
				gen.m_output << "    test eax, eax\n";
				gen.m_output << "    jz " << label << "\n";
				gen.gen_scope(stmt_if->scope);
				if (stmt_if->pred.has_value()) {
					const std::string end_label = gen.create_label();
					gen.m_output << "    jmp " << end_label << "\n";
					gen.m_output << "    " << label << ":\n";
					gen.gen_if_pred(stmt_if->pred.value(), end_label);
					gen.m_output << "    " << end_label << ":\n";
				}
				else {
					gen.m_output << "    " << label << ":\n";
				}
			}

			void operator()(const NodeStmtWhile* stmt_while) const
			{
				auto preiflab = gen.create_label();
				auto blocklab = gen.create_label();
				auto breaklab = gen.create_label();
				auto endlab = gen.create_label();
				gen.m_breaks.push_back(breaklab);
				gen.m_output << "    " << preiflab << ":\n";
				gen.gen_expr(stmt_while->expr);
				gen.m_output << "    pop eax\n";
				gen.m_output << "    test eax, eax\n";
				gen.m_output << "    jz " << endlab << "\n";
				gen.m_output << "    " << blocklab << ":\n";
				gen.gen_scope(stmt_while->scope, 0, true);
				gen.m_output << "    jmp " << preiflab << "\n";
				gen.m_output << "    " << breaklab << ":\n";
				if(gen.m_break_scopes[gen.m_break_scopes.size() - 1] != 0ULL) {
					gen.m_output << "    add esp, " << gen.m_break_scopes[gen.m_break_scopes.size() - 1] * 4 << "\n";
				}
				gen.m_output << "    " << endlab << ":\n";
				gen.m_breaks.pop_back();
				gen.m_break_scopes.pop_back();
			}

			void operator()(const NodeStmtBreak* stmt_break)
			{
				if(gen.m_breaks.size() == 0ULL) {
					gen.GeneratorError(stmt_break->def, "break without loop");
				}
				gen.m_output << "    jmp " << gen.m_breaks[gen.m_breaks.size() - 1ULL] << "\n";
			}

			void operator()(const NodeStmtStore* stmt_store) const
			{
				DataType ptype = gen.type_of_expr(stmt_store->ptr);
				if(ptype.root() != BaseDataTypePtr) {
					if(ptype.root().ptrlvl == 0ULL) {
						gen.GeneratorError(stmt_store->def, "store types missmatch\nNOTE: except `ptr`, `T`\nNOTE: but got " + ptype.to_string() + ", " + "`T`");
					}
				}
				if(stmt_store->size == 8U) {
					gen.gen_expr(stmt_store->ptr);
					gen.gen_expr(stmt_store->expr);
					gen.m_output << "    pop edx\n";
					gen.m_output << "    pop ecx\n";
					gen.m_output << "    mov byte [ecx], dl\n";
				}
				else if(stmt_store->size == 16U) {
					gen.gen_expr(stmt_store->ptr);
					gen.gen_expr(stmt_store->expr);
					gen.m_output << "    pop edx\n";
					gen.m_output << "    pop ecx\n";
					gen.m_output << "    mov word [ecx], dx\n";
				}
				else if(stmt_store->size == 32U) {
					gen.gen_expr(stmt_store->ptr);
					gen.gen_expr(stmt_store->expr);
					gen.m_output << "    pop edx\n";
					gen.m_output << "    pop ecx\n";
					gen.m_output << "    mov dword [ecx], edx\n";
				} else {
					assert(false); // unreacheable
				}
			}

			void operator()(const NodeStmtBuffer* stmt_buf) {
				int size = gen.eval(stmt_buf->size, stmt_buf->def);
				if(size % 2 != 0) {
					gen.GeneratorError(stmt_buf->def, "size of buffer must be a even number");
				}
				gen.m_var_index += (size / 4);
				gen.create_var_va_wid(stmt_buf->name, BaseDataTypeChar, stmt_buf->def);
			}

			void operator()(const NodeStmtAsm* stmt_asm) {
				gen.m_output << "    " << stmt_asm->code << "\n";
			}

			void operator()(const NodeStmtCextern* stmt_cextern) {
				if(std::find(gen.m_cexterns.begin(), gen.m_cexterns.end(), stmt_cextern->name) != gen.m_cexterns.end()) {
					return; // already in externs
				}
				gen.m_cexterns.push_back(stmt_cextern->name);
			}

			void operator()(const NodeStmtStruct* stmt_struct) {
				size_t current_type_id = (*gen.m_typeid_table_size)++;
				gen.m_typeid_table.push_back(std::make_pair(current_type_id, stmt_struct->name));
				gen.m_structs[stmt_struct->name] = { .name = stmt_struct->name, .fields = gen.compute_fields(stmt_struct->fields), .__allocator = stmt_struct->__allocator , .__fields = stmt_struct->fields, .m_typeid = gen.m_structs_count++ , .temp = stmt_struct->temp , .temps = stmt_struct->temps };
			}

			void operator()(const NodeStmtInterface* stmt_inter) {
				gen.m_interfaces[stmt_inter->name] = { .name = stmt_inter->name, .fields = gen.compute_fields(stmt_inter->fields), .__fields = stmt_inter->fields, .m_typeid = gen.m_structs_count++ };
			}

			void operator()(const NodeStmtOninit* stmt_oninit) {
				gen.__oninits.push_back(stmt_oninit->scope);
			}

			void operator()(const NodeStmtStaticAssert* stmt_st) {
				if(!static_cast<bool>(gen.eval(stmt_st->condition, stmt_st->def))) {
					gen.DiagnosticMessage(stmt_st->def, "AssertionFailed", stmt_st->msg, strlen("static_assert("));
					exit(EXIT_FAILURE);
				}
			}

			void operator()(const NodeStmtDelete* stmt_delete) {
				DataType type = gen.type_of_expr(stmt_delete->expr);
				if(!type.is_object()) {
					gen.GeneratorError(stmt_delete->def, "`delete` except object\nNOTE: but got " + gen.type_of_expr(stmt_delete->expr).to_string());
				}
				std::string objectName = type.getobjectname();
				std::optional<Struct> st = gen.struct_lookup(objectName);
				if(st.has_value()) {
					if(st.value().__allocator.has_value()) {
						gen.GeneratorWarning(stmt_delete->def, "objects of type `" + objectName + "` uses custom allocator function.\nNOTE: delete of object of this type may free youre arena-pool.");
					}
				}
				std::optional<Namespace*> nm = gen.namespace_lookup(objectName);
				if(nm.has_value()) {
					const auto& search = nm.value()->procs.find("destroy");
					if(search != nm.value()->procs.end()) {
						NodeStmtMtCall call;
						call.def = stmt_delete->def;
						call.mt = stmt_delete->expr;
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
				gen.m_output << "    call memfree\n";
				gen.m_output << "    add esp, 4\n";
			}

			void operator()(const NodeStmtRaise* stmt_raise) {
				DataType tp = gen.type_of_expr(stmt_raise->expr);
				bool has_what = false;
				if(tp.is_object()) {
					std::string oname = tp.root().getobjectname();
					std::optional<Namespace*> nm = gen.namespace_lookup(oname);
					if(nm.has_value()) {
						Namespace* nms = nm.value();
						const auto& search = nms->procs.find("what");
						if(search != nms->procs.end()) {
							Procedure proc = search->second;
							has_what = true;
							gen.m_output << "    push ";
							gen.m_output << oname << "@what\n";
						}
					}
				}
				if(!has_what) {
					gen.m_output << "    push 0\n";
				}
				gen.gen_expr(stmt_raise->expr);
				gen.m_output << "    push dword " << gen.typeid_of(tp) << '\n';
				gen.m_output << "    call __bpm_allocate_exception\n";
				gen.m_output << "    add esp, 12\n";
				gen.m_output << "    push eax\n";
				gen.m_output << "    call __bpm_throw\n";
				gen.m_output << "    add esp, 4\n";
			}

			void operator()(const NodeStmtNamespace* stmt_space) {
				std::optional<Namespace*> enm = gen.namespace_lookup(stmt_space->name);
				if(!enm.has_value()) {
					Namespace* nm = gen.m_allocator.emplace<Namespace>();
					nm->procs = {};
					nm->name = stmt_space->name;
					gen.m_cur_namespace = nm;
					gen.m_namespaces[nm->name] = nm;
				} else {
					gen.m_cur_namespace = enm.value();
				}
				for(NodeStmt* stmt : stmt_space->scope->stmts) {
					gen.gen_stmt(stmt);
				}
				gen.m_cur_namespace = NULL;
			}

			void operator()(NodeStmtImpl* stmt_impl) {
				std::optional<Namespace*> enm = gen.namespace_lookup(stmt_impl->name);
				std::string iname = stmt_impl->name;
				std::optional<Struct> h_s = gen.struct_lookup(iname);
				if(!h_s.has_value()) gen.GeneratorError(stmt_impl->def, "unkown structure name `" + iname + "`.");
				if(enm.has_value()) {
					gen.GeneratorError(stmt_impl->def, "redefenition of implementation struct `" + iname + "`.");
				}
				Namespace* nm = gen.m_allocator.emplace<Namespace>();
				nm->procs = {};
				nm->name = stmt_impl->name;
				gen.m_cur_namespace = nm;
				gen.m_namespaces[nm->name] = nm;
				for(NodeStmt* stmt : stmt_impl->scope->stmts) {
					if(!stmt_impl->temps.empty() && std::holds_alternative<NodeStmtProc*>(stmt->var)) {
						NodeStmtProc* ps = std::get<NodeStmtProc*>(stmt->var);
						std::string pname = ps->name;
						if(std::find(stmt_impl->inst.begin(), stmt_impl->inst.end(), pname) == stmt_impl->inst.end()) {
							if(ps->templates == NULL) ps->templates = gen.m_parser->m_allocator.emplace<__stdvec<std::string>>();
							ps->templates->insert(ps->templates->begin(), stmt_impl->temps.begin(), stmt_impl->temps.end());
						}
					}
					gen.gen_stmt(stmt);
				}
				gen.m_cur_namespace = NULL;
			}

			void operator()(NodeStmtNmCall* stmt_call) {
				__str_ref pname = stmt_call->name;
				__str_ref nname = stmt_call->nm;
				std::optional<Namespace*> nsp = gen.namespace_lookup(nname);
				if(!nsp.has_value()) gen.GeneratorError(stmt_call->def, "unkown namespace `" + nname + "`");
				const auto& search = nsp.value()->procs.find(pname);
				if(search == nsp.value()->procs.end()) gen.GeneratorError(stmt_call->def, "namespace `" + nname + "` doesn't have procedure `" + pname + "`");
				Procedure proc = search->second;
				if(!proc.overrides.empty()) {
					gen.resolve_overrides_tp(&proc, stmt_call->args, stmt_call->def, stmt_call->targs);
				}
				__map<std::string, DataType> temps;
				bool substituted = false;
				if(proc.templates != NULL && stmt_call->targs.empty()) {
					if(stmt_call->args.has_value()) {
						temps = gen.try_derive_templates(stmt_call->targs, proc.params, stmt_call->def, proc.templates, gen.__getargs(stmt_call->args.value()), proc);
						substituted = true;
					}
				}
				if(proc.templates != NULL && stmt_call->targs.empty()) {
					gen.GeneratorError(stmt_call->def, "procedure `" + stmt_call->name + "` excepts template arguments in <...>.");
				}
				std::string tsign;
				if(!stmt_call->targs.empty()) {
					for(int i = 0;i < static_cast<int>(stmt_call->targs.size());++i) {
						DataType t = stmt_call->targs[i];   // копия
        				gen.substitute_template(t);
        				tsign += t.sign();
					}
					Procedure* _inst_p = proc.override ? gen.m_namespaces[nname]->procs[pname].overrides[proc.overload_nth - 1] : &gen.m_namespaces[nname]->procs[pname];
					if(!_inst_p->instanceated[tsign]) {
						Generator dop_gen(gen);
						dop_gen.m_temps.emplace_back(__map<std::string, DataType>{});
						size_t counter {0};
						for(auto&& el : *proc.templates) {
							dop_gen.m_temps[dop_gen.m_temps.size() - 1][el] = stmt_call->targs[counter++];
						}
						if(substituted) gen.substitute_template_params(temps, proc.params);
						else gen.substitute_template_params(dop_gen.m_temps.back(), proc.params);
						_inst_p->instanceated[tsign] = true;
						dop_gen.m_output << nname << "@" << proc.name << tsign;
						if(proc.override) dop_gen.m_output << proc.get_sign();
						dop_gen.m_output << ":\n";
						dop_gen.m_output << "    push ebp\n";
						dop_gen.m_output << "    mov ebp, esp\n";
						dop_gen.gen_traceback_push_nm(proc, nname);
						dop_gen.m_tsigns.push_back(tsign);
						proc.mbn = nname;
						dop_gen.m_cur_proc = proc;
						dop_gen.gen_scope_sp(proc.scope, proc.from, proc);
						if(!substituted) {
							temps = std::move(dop_gen.m_temps.back());
						}
						gen.m_strings = std::move(dop_gen.m_strings);
						dop_gen.m_temps.pop_back();
						dop_gen.m_tsigns.pop_back();
						dop_gen.m_output << "    push eax\n";
						dop_gen.m_output << "    call traceback_pop\n";
						dop_gen.m_output << "    pop eax\n";
						dop_gen.m_output << "    pop ebp\n";
						dop_gen.m_output << "    ret\n\n";
						(*gen.m_result) << (dop_gen.m_output.str());
					} else {
						gen.substitute_template_params(temps, proc.params);
					}
				}
				size_t stack_allign = 0;
				if(stmt_call->args.has_value()) {
					gen.__typecheck_call(gen.__getargs(stmt_call->args.value()), proc.params, stmt_call->def, proc, &stack_allign);
				} else if(proc.params.size() != 0ULL) {
					gen.GeneratorError(stmt_call->def, "procedure `" + proc.name + "` excepts " + std::to_string(proc.params.size()) + " arguments\nNOTE: but got 0");
				}
				if(stmt_call->args.has_value()) {
					gen.gen_args(gen.__getargs(stmt_call->args.value()), proc.params);
				}
				gen.m_output << "    call " << nname << "@" << pname;
				if(!stmt_call->targs.empty()) gen.m_output << tsign;
				if(proc.override) gen.m_output << proc.get_sign();
				gen.m_output << "\n";
				if(stack_allign != 0) {
					gen.m_output << "    add esp, " << stack_allign * 4 << "\n";
				}
			}

			void operator()(const NodeStmtMtCall* stmt_call) const {
				DataType tpof = gen.type_of_expr(stmt_call->mt);
				if(!tpof.is_object() || tpof.root().link) gen.GeneratorError(stmt_call->def, "can't call method from type " + tpof.to_string() + ".");
				NodeStmtNmCall nmcall;
				nmcall.def = stmt_call->def;
				nmcall.nm = tpof.getobjectname();
				nmcall.name = stmt_call->name;
				nmcall.args = stmt_call->args;
				nmcall.targs = stmt_call->targs;
				NodeStmt stmt;
				stmt.var = &nmcall;
				gen.gen_stmt(&stmt);
			}

			void operator()(const NodeStmtConst* stmt_const) {
				gen.last_scope_cns()[stmt_const->name] = { .name = stmt_const->name , .value = gen.eval(stmt_const->expr, stmt_const->def) };
			}

			void operator()(const NodeStmtTypedef* stmt_tdef) {
				gen.last_scope_tdef()[stmt_tdef->name] = stmt_tdef->type;
			}

			void operator()(const NodeStmtTry* stmt_try) const {
				const std::string catch_lab = gen.create_label();
				const std::string end_catch = gen.create_label();
				const std::string end_lab = gen.create_label();
				gen.m_output << "    mov edx, dword [tmp_p]\n";
				gen.m_output << "    add edx, 4\n";
				gen.m_output << "    mov dword [tmp_stor+edx], ebp\n";
				gen.m_output << "    add edx, 4\n";
				gen.m_output << "    mov dword [tmp_stor+edx], esp\n";
				gen.m_output << "    mov dword [tmp_p], edx\n";
				gen.m_output << "    inc dword [__exception_bufs_lvl]\n";
				gen.m_output << "    mov eax, dword [__exception_bufs_lvl]\n";
        		gen.m_output << "    imul eax, eax, 64\n";
        		gen.m_output << "    add eax, __exception_bufs\n";
        		gen.m_output << "    push eax\n";
        		gen.m_output << "    call _setjmp\n";
				gen.m_output << "    add esp, 4\n";
				gen.m_output << "    cmp eax, 1\n";
				gen.m_output << "    jnz " << end_catch << '\n';
				gen.m_output << "    " << catch_lab << ":\n";
				gen.m_output << "    mov edx, dword [tmp_p]\n";
				gen.m_output << "    mov esp, dword [tmp_stor+edx]\n";
				gen.m_output << "    sub edx, 4\n";
				gen.m_output << "    mov ebp, dword [tmp_stor+edx]\n";
				gen.m_output << "    sub edx, 4\n";
				gen.m_output << "    mov dword [tmp_p], edx\n";
				gen.create_var_va(stmt_try->name, stmt_try->type, stmt_try->def);
				Var vr = gen.var_lookup_cs(stmt_try->name).value();
				gen.m_output << "    call __bpm_get_current_exception\n";
				gen.m_output << "    mov dword [ebp-" << vr.stack_loc << "], eax\n";
				gen.gen_scope(stmt_try->_catch);
				gen.last_scope().erase(gen.last_scope().find(stmt_try->name));
				gen.m_output << "    jmp " << end_lab << "\n";
				gen.m_output << "    " << end_catch << ":\n";
				gen.m_output << "    push dword " << gen.typeid_of(stmt_try->type) << '\n';
				gen.m_output << "    call __bpm_start_catch\n";
				gen.m_output << "    add esp, 4\n";
				gen.gen_scope(stmt_try->_try);
				gen.m_output << "    call __bpm_end_catch\n";
				gen.m_output << "    " << end_lab << ":\n";
			}
		};

		StmtVisitor visitor { .gen = *this };
		std::visit(visitor, stmt->var);
	}

	[[nodiscard]] std::string gen_prog()
	{
		m_consts.push_back({});
		m_typedefs.push_back({});
		std::stringstream result;
		m_result = &result;
		m_string_index = static_cast<size_t*>(malloc(sizeof(size_t)));
		m_label_count = static_cast<size_t*>(malloc(sizeof(size_t)));
		m_typeid_table_size = static_cast<size_t*>(malloc(sizeof(size_t)));
		*m_typeid_table_size = 5ULL;
		*m_label_count = 0ULL;
		*m_string_index = 0ULL;
		result << "section .text\n\n";
		result << "global main\n\n";

		for (const NodeStmt* stmt : m_prog->stmts) {
			gen_stmt(stmt);
		}

		yforeach(m_cexterns) {
			result << "extern " << m_cexterns[i] << "\n";
		}

		result << "\n";

		result << "main:\n";
		result << "    mov dword [stack_base], ebp\n";
		result << "    push dword " << (*m_typeid_table_size) * 4ULL << '\n';
		result << "    call malloc\n";
		result << "    add esp, 4\n";
		result << "    mov dword [__type_id_table], eax\n";
		m_init_typeid_table(result);
		result << "    call __main\n";
		result << "    push 0\n";
		result << "    call ExitProcess@4\n";

		result << "\n";

		bool __has_oninits = __oninits.size() != 0ULL;

		m_output << "_BPM_init_:\n";
		if(__has_oninits) {
			m_output << "    push ebp\n";
			m_output << "    mov ebp, esp\n";
		}
		m_output << "    mov dword [tmp_p], dword 0x0\n";
		for(const NodeScope* scope : __oninits) {
			gen_scope(scope);
		}
		if(__has_oninits) {
			m_output << "    pop ebp\n";
		}
		m_output << "    ret\n\n\n";

		m_output << "\nsection .data\n";
		m_output << "    numfmt: db \"%d\", 0x0\n";
		m_output << "    numfmtnl: db \"%d\", 0xa, 0x0\n";
		m_output << "    strfmt: db \"%s\", 0x0\n";
		for(auto&& pairs : m_strings) {
			const String& cur_s = pairs.second;
			m_output << "    s_" << static_cast<int>(cur_s.index) << ": db ";
			std::stringstream hexstr;
			for(int j = 0;j < static_cast<int>(cur_s.value.length());++j) {
				hexstr << "0x" << std::hex << static_cast<int>(cur_s.value[j]) << ", ";
			}
			m_output << hexstr.str();
			hexstr.clear();
			m_output << "0x0\n";
		}
		m_output << "\nsection .bss\n";
		m_output << "    tmp_stor: resd 1024\n";
		m_output << "    tmp_p: resd 1\n";
		for(auto&& pairs : m_global_vars) {
			const GVar& ivar = pairs.second;
			m_output << "    v_" << ivar.name << ": resd 1\n";
		}
		result << m_output.str();
		return result.str();
	}

	void get_props_from_parser(Parser* parser) noexcept {
		m_parser = parser;
		m_lines = &(parser->m_lines);
	}

private:
	void m_init_typeid_table(std::stringstream& result) noexcept {
		size_t counter {0};
		result << "    mov ecx, dword [__type_id_table]\n";
		for(auto&& tp : m_typeid_table) {
			size_t _index;
			__str_ref value = tp.second;
			std::optional<String> str = string_lookup(value);
			if(!str.has_value()) {
				size_t index = (*m_string_index)++;
				m_strings[value] = { .value = value, .index = index };
				_index = index;
			} else {
				_index = str.value().index;
			}
			result << "    mov dword [ecx+" << tp.first * 4ULL << "], s_" << _index << "\n";
			counter++;
		}
	}
	inline void push(__str_ref reg) noexcept
	{
		m_output << "    push " << reg << "\n";
	}

	inline void pop(__str_ref reg) noexcept
	{
		m_output << "    pop " << reg << "\n";
	}

	void begin_scope(int fsz)
	{
		if(fsz != 0) {
			m_output << "    sub esp, " << fsz * 4 << "\n";
		}
		m_consts.push_back({});
		m_vars.push_back({});
		m_typedefs.push_back({});
		m_scopes_vi.push_back(m_var_index);
		m_scopes.push_back(fsz);
	}

	void end_scope()
	{
		if(m_scopes[m_scopes.size() - 1ULL] != 0ULL) {
			m_output << "    add esp, " << m_scopes[m_scopes.size() - 1ULL] * 4 << "\n";
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
		if(in_namespace()) m_output << "    __" << m_cur_namespace->name << "@";
		else m_output << "    __";
		if(!proc.mbn.empty()) m_output << proc.mbn << "@";
		m_output << proc.name;
		if(!m_tsigns.empty()) m_output << m_tsigns.back();
		if(proc.override) {
			m_output << proc.get_sign();
		}
		m_output << "@ret:\n";
		if(m_scopes[m_scopes.size() - 1ULL] != 0ULL) {
			m_output << "    add esp, " << m_scopes[m_scopes.size() - 1ULL] * 4 << "\n";
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
		yforeach(m_scopes) {
			if(i == 0) continue;
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

	const NodeProg* m_prog;
	const AsmGen asmg { .gen = this };
	std::stringstream* m_result;
	std::stringstream		 m_output;
	__map<std::string, __stdvec<std::string>>* m_lines;
	__stdvec<__map<std::string, Var>> m_vars;
	__map<std::string, String> m_strings;
	__map<std::string, Procedure> m_procs;
	__map<std::string, Struct> m_structs;
	__map<std::string, GVar> m_global_vars;
	__map<std::string, Interface> m_interfaces;
	__map<std::string, Namespace*> m_namespaces;
	Namespace* m_cur_namespace;
	ArenaAllocator m_allocator;
	Parser* m_parser;
	size_t m_structs_count = 5;
	std::optional<Procedure> m_cur_proc;
	std::vector<std::string> m_breaks;
	VectorSim<size_t>		m_scopes;
	VectorSim<size_t>		m_scopes_vi;
	VectorSim<size_t>		m_break_scopes;
	__stdvec<__map<std::string, Constant>> m_consts;
	__stdvec<__map<std::string, DataType>> m_typedefs;
	__stdvec<std::pair<size_t, std::string>> m_typeid_table {
		{TYPEID_INT,  "int"},
		{TYPEID_PTR,  "ptr"},
		{TYPEID_VOID, "void"},
		{TYPEID_ANY,  "any"},
		{TYPEID_CHAR, "char"},
	};
	std::vector<std::string> m_cexterns {
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
		"traceback_pop"
	};
	__stdvec<NodeScope*> __oninits;
	__stdvec<std::string> m_tsigns;
	__stdvec<__map<std::string, DataType>> m_temps;
	size_t* m_string_index;
	size_t CTX_IOTA = 0ULL;
	size_t m_var_index = 0ULL;
	size_t* m_label_count;
	size_t* m_typeid_table_size;
};