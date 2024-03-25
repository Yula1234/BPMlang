#pragma once

#include <algorithm>
#include <cassert>

#include "parser.hpp"

class Generator {
public:
	struct Var {
		std::string name {};
		size_t stack_loc {};
		DataType type;
	};
	struct Procedure {
		std::string name {};
		std::vector<std::pair<std::string, DataType>> params {};
		DataType rettype;
		size_t stack_allign;
		std::vector<ProcAttr> attrs;
		Token def;
	};
	struct String {
		std::string value {};
		size_t index {};
	};
	struct Struct {
		std::string name;
		std::vector<std::pair<std::string, DataType>> fields;
	};
	explicit Generator(NodeProg prog)
		: m_prog(std::move(prog))
	{
	}

	std::optional<Var> var_lookup(std::string name) {
		yforeach(m_vars) {
			if(m_vars[i].name == name) {
				return m_vars[i];
			}
		}
		return std::nullopt;
	}

	std::optional<Procedure> proc_lookup(std::string name) {
		yforeach(m_procs) {
			if(m_procs[i].name == name) {
				return m_procs[i];
			}
		}
		return std::nullopt;
	}

	std::optional<Struct> struct_lookup(std::string name) {
		yforeach(m_structs) {
			if(m_structs[i].name == name) {
				return m_structs[i];
			}
		}
		return std::nullopt;
	}

	std::optional<std::pair<size_t, DataType>> field_lookup(Struct st, std::string field) {
		for(int i = 0;i < static_cast<int>(st.fields.size());++i) {
			if(st.fields[i].first == field) {
				return std::make_pair(static_cast<size_t>(i), st.fields[i].second);
			}
		}
		return std::nullopt;
	}

	std::optional<String> string_lookup(std::string svalue) {
		yforeach(m_strings) {
			if(m_strings[i].value == svalue) {
				return m_strings[i];
			}
		}
		return std::nullopt;
	}

	void GeneratorError(Token tok, std::string msg) {
		putloc(tok);
		std::cout << " ERROR: " << msg << "\n";
		exit(EXIT_FAILURE);
	}

	DataType type_of_dot(NodeBinExprDot* dot) {
		DataType otype = type_of_expr(dot->lhs);
		if(std::holds_alternative<NodeTerm*>(dot->rhs->var)) {
			NodeTerm* id = std::get<NodeTerm*>(dot->rhs->var);
			if(!std::holds_alternative<NodeTermIdent*>(id->var)) {
				return DataTypeVoid;
			}
			NodeTermIdent* tid = std::get<NodeTermIdent*>(id->var);
			Token ident = tid->ident;
			std::string field_name = ident.value.value();
			if(!otype.is_object) {
				return DataTypeVoid;
			}
			std::string struct_name = otype.getobjectname();
			Struct st = struct_lookup(struct_name).value();
			std::optional<std::pair<size_t, DataType>> field = field_lookup(st, field_name);
			if(!field.has_value()) {
				return DataTypeVoid;
			}
			return field.value().second;
		} else {
			return DataTypeVoid;
		}
	}

	DataType type_of_expr(const NodeExpr* expr) {
		if(holds_alternative<NodeTerm*>(expr->var)) {
			NodeTerm* term = std::get<NodeTerm*>(expr->var);
			if(std::holds_alternative<NodeTermIntLit*>(term->var)) {
				return make_int_type();
			}
			if(std::holds_alternative<NodeTermStrLit*>(term->var)) {
				return make_ptr_type();
			}
			if(std::holds_alternative<NodeTermCast*>(term->var)) {
				return std::get<NodeTermCast*>(term->var)->type;
			}
			if(std::holds_alternative<NodeTermRd*>(term->var)) {
				return make_int_type();
			}
			if(std::holds_alternative<NodeTermParen*>(term->var)) {
				return type_of_expr(std::get<NodeTermParen*>(term->var)->expr);
			}
			if(std::holds_alternative<NodeTermAmpersand*>(term->var)) {
				return make_ptr_type();
			}
			if(std::holds_alternative<NodeTermCall*>(term->var)) {
				NodeTermCall* call = std::get<NodeTermCall*>(term->var);
				std::string name = call->name;
				std::optional<Procedure> proc = proc_lookup(name);
				if(proc.has_value()) {
					return proc.value().rettype;
				}
				std::optional<Struct> st = struct_lookup(call->name);
				if(st.has_value()) {
					DataType dt = st.value().name;
					return dt;
				}
				return DataTypeVoid;
			}
			if(std::holds_alternative<NodeTermIdent*>(term->var)) {
				std::optional<Var> svar = var_lookup(std::get<NodeTermIdent*>(term->var)->ident.value.value());
				if(svar.has_value()) {
					return svar.value().type;
				}
				GeneratorError(std::get<NodeTermIdent*>(term->var)->ident, "unkown word `" + std::get<NodeTermIdent*>(term->var)->ident.value.value() + "`");
			}
		}
		static_assert(BinaryOpsCount == 7,
					"\n	 Impl type_of for new binop");
		if(holds_alternative<NodeBinExpr*>(expr->var)) {
			NodeBinExpr* binex = std::get<NodeBinExpr*>(expr->var);
			if(std::holds_alternative<NodeBinExprAdd*>(binex->var)) {
				return type_of_expr(std::get<NodeBinExprAdd*>(binex->var)->lhs);
			}
			if(std::holds_alternative<NodeBinExprMulti*>(binex->var)) {
				return type_of_expr(std::get<NodeBinExprMulti*>(binex->var)->lhs);
			}
			if(std::holds_alternative<NodeBinExprSub*>(binex->var)) {
				return type_of_expr(std::get<NodeBinExprSub*>(binex->var)->lhs);
			}
			if(std::holds_alternative<NodeBinExprDiv*>(binex->var)) {
				return type_of_expr(std::get<NodeBinExprDiv*>(binex->var)->lhs);
			}
			if(std::holds_alternative<NodeBinExprMod*>(binex->var)) {
				return type_of_expr(std::get<NodeBinExprMod*>(binex->var)->lhs);
			}
			if(std::holds_alternative<NodeBinExprEqEq*>(binex->var)) {
				return type_of_expr(std::get<NodeBinExprEqEq*>(binex->var)->lhs);
			}
			if(std::holds_alternative<NodeBinExprNotEq*>(binex->var)) {
				return type_of_expr(std::get<NodeBinExprNotEq*>(binex->var)->lhs);
			}
			if(std::holds_alternative<NodeBinExprLess*>(binex->var)) {
				return type_of_expr(std::get<NodeBinExprLess*>(binex->var)->lhs);
			}
			if(std::holds_alternative<NodeBinExprAbove*>(binex->var)) {
				return type_of_expr(std::get<NodeBinExprAbove*>(binex->var)->lhs);
			}
			if(std::holds_alternative<NodeBinExprDot*>(binex->var)) {
				NodeBinExprDot* dot = std::get<NodeBinExprDot*>(binex->var);
				return type_of_dot(dot);
			}  
		}
		assert(false);
	}

	bool typecheck_bin_expr(const NodeBinExpr* expr) {
		static_assert(BinaryOpsCount == 7,
					"\n	 Impl typecheck for new binop");
		if(std::holds_alternative<NodeBinExprAdd*>(expr->var)) {
			NodeBinExprAdd* add = std::get<NodeBinExprAdd*>(expr->var);
			return type_of_expr(add->lhs) == type_of_expr(add->rhs);
		} else if(std::holds_alternative<NodeBinExprSub*>(expr->var)) {
			NodeBinExprSub* sub = std::get<NodeBinExprSub*>(expr->var);
			return type_of_expr(sub->lhs) == type_of_expr(sub->rhs);
		} else if(std::holds_alternative<NodeBinExprMulti*>(expr->var)) {
			NodeBinExprMulti* mul = std::get<NodeBinExprMulti*>(expr->var);
			return type_of_expr(mul->lhs) == type_of_expr(mul->rhs);
		} else if(std::holds_alternative<NodeBinExprDiv*>(expr->var)) {
			NodeBinExprDiv* div = std::get<NodeBinExprDiv*>(expr->var);
			return type_of_expr(div->lhs) == type_of_expr(div->rhs);
		} else if(std::holds_alternative<NodeBinExprMod*>(expr->var)) {
			NodeBinExprMod* md = std::get<NodeBinExprMod*>(expr->var);
			return type_of_expr(md->lhs) == type_of_expr(md->rhs);
		} else if(std::holds_alternative<NodeBinExprEqEq*>(expr->var)) {
			NodeBinExprEqEq* eqeq = std::get<NodeBinExprEqEq*>(expr->var);
			return type_of_expr(eqeq->lhs) == type_of_expr(eqeq->rhs);
		} else if(std::holds_alternative<NodeBinExprNotEq*>(expr->var)) {
			NodeBinExprNotEq* nq = std::get<NodeBinExprNotEq*>(expr->var);
			return type_of_expr(nq->lhs) == type_of_expr(nq->rhs);
		} else if(std::holds_alternative<NodeBinExprLess*>(expr->var)) {
			NodeBinExprLess* less = std::get<NodeBinExprLess*>(expr->var);
			return type_of_expr(less->lhs) == type_of_expr(less->rhs);
		} else if(std::holds_alternative<NodeBinExprAbove*>(expr->var)) {
			NodeBinExprAbove* above = std::get<NodeBinExprAbove*>(expr->var);
			return type_of_expr(above->lhs) == type_of_expr(above->rhs);
		} else if(std::holds_alternative<NodeBinExprDot*>(expr->var)) {
			return true;
		} else {
			assert(false);
		}
	}

	void typecheck_bin_expr_err(const NodeBinExpr* expr, std::string IRexpr) {
		static_assert(BinaryOpsCount == 7,
					"\n	 Impl typecheck_err for new binop");
		if(!typecheck_bin_expr(expr)) {
			DataType ltype;
			DataType rtype;
			if(std::holds_alternative<NodeBinExprAdd*>(expr->var)) {
				const NodeBinExprAdd* add = std::get<NodeBinExprAdd*>(expr->var);
				ltype = type_of_expr(add->lhs);
				rtype = type_of_expr(add->rhs);
			} else if(std::holds_alternative<NodeBinExprSub*>(expr->var)) {
				const NodeBinExprSub* sub = std::get<NodeBinExprSub*>(expr->var);
				ltype = type_of_expr(sub->lhs);
				rtype = type_of_expr(sub->rhs);
			} else if(std::holds_alternative<NodeBinExprMulti*>(expr->var)) {
				const NodeBinExprMulti* mul = std::get<NodeBinExprMulti*>(expr->var);
				ltype = type_of_expr(mul->lhs);
				rtype = type_of_expr(mul->rhs);
			} else if(std::holds_alternative<NodeBinExprDiv*>(expr->var)) {
				const NodeBinExprDiv* div = std::get<NodeBinExprDiv*>(expr->var);
				ltype = type_of_expr(div->lhs);
				rtype = type_of_expr(div->rhs);
			} else if(std::holds_alternative<NodeBinExprMod*>(expr->var)) {
				const NodeBinExprMod* md = std::get<NodeBinExprMod*>(expr->var);
				ltype = type_of_expr(md->lhs);
				rtype = type_of_expr(md->rhs);
			} else if(std::holds_alternative<NodeBinExprEqEq*>(expr->var)) {
				const NodeBinExprEqEq* eqeq = std::get<NodeBinExprEqEq*>(expr->var);
				ltype = type_of_expr(eqeq->lhs);
				rtype = type_of_expr(eqeq->rhs);
			} else if(std::holds_alternative<NodeBinExprLess*>(expr->var)) {
				const NodeBinExprLess* less = std::get<NodeBinExprLess*>(expr->var);
				ltype = type_of_expr(less->lhs);
				rtype = type_of_expr(less->rhs);
			} else if(std::holds_alternative<NodeBinExprAbove*>(expr->var)) {
				const NodeBinExprAbove* above = std::get<NodeBinExprAbove*>(expr->var);
				ltype = type_of_expr(above->lhs);
				rtype = type_of_expr(above->rhs);
			} else if(std::holds_alternative<NodeBinExprNotEq*>(expr->var)) {
				const NodeBinExprNotEq* nq = std::get<NodeBinExprNotEq*>(expr->var);
				ltype = type_of_expr(nq->lhs);
				rtype = type_of_expr(nq->rhs);
			} else {
				assert(false);
			}
			if(!(ltype == DataTypePtr && rtype == DataTypeInt)) {
				GeneratorError(expr->def, "can't use `" + IRexpr + "` for types " + dt_to_string(ltype) + " and " + dt_to_string(rtype));
			}
		}
	}

	void gen_term(const NodeTerm* term, bool lvalue = false)
	{
		struct TermVisitor {
			Generator& gen;
			bool lvalue;

			void operator()(const NodeTermIntLit* term_int_lit) const
			{
				gen.push(term_int_lit->int_lit.value.value());
			}

			void operator()(const NodeTermRd* term_rd) const
			{
				if(term_rd->size == 8) {
					gen.gen_expr(term_rd->expr);
					gen.m_output << "	pop edx\n";
					gen.m_output << "	xor ecx, ecx\n";
					gen.m_output << "	mov cl, byte [edx]\n";
					gen.m_output << "	push ecx\n";
				} else if(term_rd->size == 16) {
					gen.gen_expr(term_rd->expr);
					gen.m_output << "	pop edx\n";
					gen.m_output << "	xor ecx, ecx\n";
					gen.m_output << "	mov cx, word [edx]\n";
					gen.m_output << "	push ecx\n";
				} else if(term_rd->size == 32) {
					gen.gen_expr(term_rd->expr);
					gen.m_output << "	pop edx\n";
					gen.m_output << "	push dword [edx]\n";
				} else {
					assert(false); // unreacheable
				}
			}

			void operator()(const NodeTermCast* term_cast) const
			{
				gen.gen_expr(term_cast->expr);
			}

			void operator()(const NodeTermStrLit* term_str_lit) const
			{
				std::string value = term_str_lit->str_lit.value.value();
				std::optional<String> str = gen.string_lookup(value);
				if(!str.has_value()) {
					size_t index = gen.m_strings.size();
					gen.m_strings.push_back({ .value = value, .index = index});
					gen.m_output << "	push s_" << index << "\n";
					return;
				}
				gen.m_output << "	push s_" << str.value().index << "\n";
			}

			void operator()(const NodeTermAmpersand* term_amp) const
			{
				gen.gen_expr(term_amp->expr, true);
			}

			void operator()(const NodeTermIdent* term_ident) const
			{
				std::optional<Var> it = gen.var_lookup(term_ident->ident.value.value());
				if(it.has_value()) {
					if(lvalue) {
						gen.m_output << "	mov edx, ebp\n";
						gen.m_output << "	sub edx, " << it.value().stack_loc << "\n";
						gen.m_output << "	push edx\n";
					} else {
						std::stringstream offset;
						offset << "dword [ebp-" << it.value().stack_loc << "]";
						gen.push(offset.str());
					}
					return;
				}
				gen.GeneratorError(term_ident->ident, "unkown word `" + term_ident->ident.value.value() + "`");
			}

			void operator()(const NodeTermParen* term_paren) const
			{
				gen.gen_expr(term_paren->expr);
			}

			void operator()(const NodeTermCall* term_call) const
			{
				const std::string name = term_call->def.value.value();
				std::optional<Procedure> proc = gen.proc_lookup(name);
				if(proc.has_value()) {
					size_t stack_allign = 0;
					if(term_call->args.has_value()) {
						if(proc.value().params.size() == 0) {
							gen.GeneratorError(term_call->def, "procedure `" + name + "` don't excepts any arguments");
						}
						NodeExpr* args = term_call->args.value();
						if(std::holds_alternative<NodeBinExpr*>(args->var)) {
							NodeBinExpr* cexpr = std::get<NodeBinExpr*>(args->var);
							if(std::holds_alternative<NodeBinExprArgs*>(cexpr->var)) {
								std::vector<NodeExpr*> pargs = get<NodeBinExprArgs*>(cexpr->var)->args;
								if(pargs.size() != proc.value().params.size()) {
									gen.GeneratorError(term_call->def, "procedure `" + name + "` excepts " + std::to_string(proc.value().params.size()) + " arguments\nNOTE: but got " + std::to_string(pargs.size()));
								}
								for(int i = 0;i < static_cast<int>(pargs.size());++i) {
									if(gen.type_of_expr(pargs[i]) != proc.value().params[i].second) {
										gen.GeneratorError(term_call->def, "procedure `" + name + "`\nexcept type " + dt_to_string(proc.value().params[i].second) + " at " + std::to_string(i) + " argument\nNOTE: but found type " + dt_to_string(gen.type_of_expr(pargs[i])));
									}
								}
								stack_allign += pargs.size();
							}
						} else {
							if(gen.type_of_expr(args) != proc.value().params[0].second) {
								gen.GeneratorError(term_call->def, "procedure `" + name + "`\nexcept type " + dt_to_string(proc.value().params[0].second) + " at 0 argument\nNOTE: but found type " + dt_to_string(gen.type_of_expr(args)));
							}
							if(proc.value().params.size() != 1U) {
								gen.GeneratorError(term_call->def, "procedure `" + name + "` excepts " + std::to_string(proc.value().params.size()) + " arguments\nNOTE: but got 0");
							}
							stack_allign++;
						}
					} else {
						if(proc.value().params.size() != 0U) {
							gen.GeneratorError(term_call->def, "procedure `" + name + "` excepts " + std::to_string(proc.value().params.size()) + " args\nNOTE: but got 0");
						}
					}
					if(term_call->args.has_value()) {
						gen.gen_expr(term_call->args.value());
					}
					gen.m_output << "	call " << name << "\n";
					if(stack_allign != 0) {
						gen.m_output << "	add esp, " << stack_allign * 4 << "\n";
					}
					if(proc.value().rettype == DataTypeVoid) {
						gen.GeneratorError(term_call->def, "using void function as expression");
					}
					gen.m_output << "	push eax\n";
					return;
				}
				std::optional<Struct> st = gen.struct_lookup(term_call->name);
				if(st.has_value()) {
					size_t objectSize = st.value().fields.size();
					if(objectSize == 0U) {
						gen.m_output << "	push dword 0\n";
						return;
					}
					gen.m_output << "	push dword " << objectSize * 4U << "\n";
					gen.m_output << "	call malloc\n";
					gen.m_output << "	add esp, 4\n";
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
						gen.m_output << "	mov dword [tmp_stor], eax\n";
						for(int i = 0;i < static_cast<int>(iargs.size());++i) {
							DataType itype = gen.type_of_expr(iargs[i]);
							DataType ftype = st.value().fields[i].second;
							if(itype != ftype) {
								gen.GeneratorError(term_call->def, "missmatch in initializers types for field nth `" + std::to_string(i + 1) + "`\nNOTE: field name - `" + st.value().fields[i].first + "`" + "\nNOTE: excepted " + ftype.to_string() + "\nNOTE: but got " + itype.to_string());
							}
							gen.gen_expr(iargs[i]);
							gen.m_output << "	pop ecx\n";
							gen.m_output << "	mov edx, dword [tmp_stor]\n";
							gen.m_output << "	mov dword [edx+" << i * 4 << "], ecx\n";
						}
					}
					if(!eax_break) {
						gen.m_output << "	push eax\n";
					} else {
						gen.m_output << "	push dword [tmp_stor]\n";
					}
					return;
				}
				gen.GeneratorError(term_call->def, "unkown procedure `" + name + "`");
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
				gen.gen_expr(sub->rhs);
				gen.gen_expr(sub->lhs);
				gen.pop("eax");
				gen.pop("ebx");
				gen.m_output << "	sub eax, ebx\n";
				gen.push("eax");
			}

			void operator()(const NodeBinExprAdd* add) const
			{
				gen.gen_expr(add->rhs);
				gen.gen_expr(add->lhs);
				gen.pop("eax");
				gen.pop("ebx");
				gen.m_output << "	add eax, ebx\n";
				gen.push("eax");
			}

			void operator()(const NodeBinExprMulti* multi) const
			{
				gen.gen_expr(multi->rhs);
				gen.gen_expr(multi->lhs);
				gen.pop("eax");
				gen.pop("ebx");
				gen.m_output << "	mul ebx\n";
				gen.push("eax");
			}

			void operator()(const NodeBinExprDiv* div) const
			{
				gen.gen_expr(div->rhs);
				gen.gen_expr(div->lhs);
				gen.pop("eax");
				gen.pop("ebx");
				gen.m_output << "	xor edx, edx\n";
				gen.m_output << "	div ebx\n";
				gen.push("eax");
				gen.m_output << "	mov edx, ecx\n";
			}

			void operator()(const NodeBinExprMod* md) const
			{
				gen.gen_expr(md->rhs);
				gen.gen_expr(md->lhs);
				gen.pop("eax");
				gen.pop("ebx");
				gen.m_output << "	xor edx, edx\n";
				gen.m_output << "	div ebx\n";
				gen.push("edx");
			}

			void operator()(const NodeBinExprEqEq* eqeq) const
			{
				gen.gen_expr(eqeq->rhs);
				gen.gen_expr(eqeq->lhs);
				gen.m_output << "	mov edx, 0\n";
				gen.m_output << "	mov ecx, 1\n";
				gen.m_output << "	pop ebx\n";
				gen.m_output << "	pop eax\n";
				gen.m_output << "	cmp eax, ebx\n";
				gen.m_output << "	cmove edx, ecx\n";
				gen.m_output << "	push edx\n";
			}

			void operator()(const NodeBinExprNotEq* nq) const
			{
				gen.gen_expr(nq->rhs);
				gen.gen_expr(nq->lhs);
				gen.m_output << "	mov edx, 0\n";
				gen.m_output << "	mov ecx, 1\n";
				gen.m_output << "	pop ebx\n";
				gen.m_output << "	pop eax\n";
				gen.m_output << "	cmp eax, ebx\n";
				gen.m_output << "	cmovne edx, ecx\n";
				gen.m_output << "	push edx\n";
			}

			void operator()(const NodeBinExprLess* less) const
			{
				gen.gen_expr(less->lhs);
				gen.gen_expr(less->rhs);
				gen.m_output << "	mov edx, 0\n";
				gen.m_output << "	mov ecx, 1\n";
				gen.m_output << "	pop ebx\n";
				gen.m_output << "	pop eax\n";
				gen.m_output << "	cmp eax, ebx\n";
				gen.m_output << "	cmovc edx, ecx\n";
				gen.m_output << "	push edx\n";
			}

			void operator()(const NodeBinExprAbove* above) const
			{
				gen.gen_expr(above->lhs);
				// DON'T COPY PASTE
				// IN > FIRST GENERATE LEFT OPERAND
				gen.gen_expr(above->rhs);
				gen.m_output << "	mov edx, 0\n";
				gen.m_output << "	mov ecx, 1\n";
				gen.m_output << "	pop ebx\n";
				gen.m_output << "	pop eax\n";
				gen.m_output << "	cmp eax, ebx\n";
				gen.m_output << "	cmova edx, ecx\n";
				gen.m_output << "	push edx\n";
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
					if(!otype.is_object) {
						gen.GeneratorError(base->def, "bellow `.` except expression of type any object\nNOTE: but got " + otype.to_string());
					}
					std::string struct_name = otype.getobjectname();
					Struct st = gen.struct_lookup(struct_name).value();
					std::optional<std::pair<size_t, DataType>> field = gen.field_lookup(st, field_name);
					if(!field.has_value()) {
						gen.GeneratorError(base->def, "object of type `" + otype.to_string() + "` doesn`t have field `" + field_name + "`");
					}
					size_t field_offset = field.value().first;
					gen.gen_expr(dot->lhs);
					if(lvalue) {
						gen.m_output << "	pop ecx\n";
						if(field_offset != 0U) {
							gen.m_output << "	add ecx, " << field_offset * 4U << "\n";
						}
						gen.m_output << "	push ecx\n";
					} else {
						gen.m_output << "	pop ecx\n";
						if(field_offset != 0U) {
							gen.m_output << "	push dword [ecx+" << field_offset * 4U << "]\n";
						} else {
						   gen.m_output << "	push dword [ecx]\n"; 
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
		std::string bin_str = "";
		if(std::holds_alternative<NodeBinExprAdd*>(bin_expr->var)) {
			bin_str = "+";
		} else if(std::holds_alternative<NodeBinExprSub*>(bin_expr->var)) {
			bin_str = "-";
		} else if(std::holds_alternative<NodeBinExprMulti*>(bin_expr->var)) {
			bin_str = "*";
		} else if(std::holds_alternative<NodeBinExprDiv*>(bin_expr->var)) {
			bin_str = "/";
		} else if(std::holds_alternative<NodeBinExprMod*>(bin_expr->var)) {
			bin_str = "%";
		} else if(std::holds_alternative<NodeBinExprEqEq*>(bin_expr->var)) {
			bin_str = "==";
		} else if(std::holds_alternative<NodeBinExprNotEq*>(bin_expr->var)) {
			bin_str = "!=";
		} else if(std::holds_alternative<NodeBinExprLess*>(bin_expr->var)) {
			bin_str = "<";
		} else if(std::holds_alternative<NodeBinExprAbove*>(bin_expr->var)) {
			bin_str = ">";
		}
		if(!std::holds_alternative<NodeBinExprArgs*>(bin_expr->var)) {
			typecheck_bin_expr_err(bin_expr, bin_str);
		}
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

	size_t collect_alligns(const NodeScope* scope) {
		size_t fsz = 0U;
		for (const NodeStmt* stmt : scope->stmts) {
			if(std::holds_alternative<NodeStmtIf*>(stmt->var)) {
				NodeStmtIf* ifstmt = std::get<NodeStmtIf*>(stmt->var);
				fsz += collect_alligns(ifstmt->scope);
				if(ifstmt->pred.has_value()) {
					NodeIfPred* pred = ifstmt->pred.value();
					if(std::holds_alternative<NodeIfPredElif*>(pred->var)) {
						NodeIfPredElif* pelif = std::get<NodeIfPredElif*>(pred->var);
						fsz += collect_alligns(pelif->scope);
					}
					else if(std::holds_alternative<NodeIfPredElse*>(pred->var)) {
						NodeIfPredElse* pelse = std::get<NodeIfPredElse*>(pred->var);
						fsz += collect_alligns(pelse->scope);
					}
				}
			}
			else if(std::holds_alternative<NodeStmtWhile*>(stmt->var)) {
				NodeStmtWhile* whstmt = std::get<NodeStmtWhile*>(stmt->var);
				fsz += collect_alligns(whstmt->scope);
			}
			else if(std::holds_alternative<NodeStmtLet*>(stmt->var)) {
				fsz += 1;
			}
			else if(std::holds_alternative<NodeStmtLetNoAssign*>(stmt->var)) {
				fsz += 1;
			}
			else if(std::holds_alternative<NodeStmtBuffer*>(stmt->var)) {
				fsz += std::get<NodeStmtBuffer*>(stmt->var)->size / 4U;
			}
		}
		return fsz;
	}

	void gen_scope(const NodeScope* scope)
	{
		begin_scope();
		for (const NodeStmt* stmt : scope->stmts) {
			gen_stmt(stmt);
		}
		end_scope();
	}

	void gen_scope_fsz(const NodeScope* scope, const int psizes = 0)
	{
		size_t fsz = collect_alligns(scope);
		begin_scope_fsz(fsz + static_cast<size_t>(psizes));
		for (const NodeStmt* stmt : scope->stmts) {
			gen_stmt(stmt);
		}
		end_scope_fsz(fsz + static_cast<size_t>(psizes));
	}

	void create_var(const std::string name, NodeExpr* value, Token where) {
		std::optional<Var> ivar = var_lookup(name);
		if(ivar.has_value()) {
			GeneratorError(where, "name `" + name + "` already in use");
		}
		DataType vartype = type_of_expr(value);
		m_vars.push_back({ .name = name, .stack_loc = ++m_var_index * 4 , .type = vartype });
		gen_expr(value);
		m_output << "	pop ecx\n";
		m_output << "	mov dword [ebp-" << m_var_index * 4 << "], ecx\n";
	}

	void create_var_va(const std::string name, DataType type, Token where) {
		std::optional<Var> ivar = var_lookup(name);
		if(ivar.has_value()) {
			GeneratorError(where, "name `" + name + "` already in use");
		}
		m_vars.push_back({ .name = name, .stack_loc = ++m_var_index * 4 , .type = type });
	}

	void create_var_va_wid(const std::string name, DataType type, Token where) {
		std::optional<Var> ivar = var_lookup(name);
		if(ivar.has_value()) {
			GeneratorError(where, "name `" + name + "` already in use");
		}
		m_vars.push_back({ .name = name, .stack_loc = m_var_index * 4 , .type = type });
	}

	void gen_if_pred(const NodeIfPred* pred, const std::string& end_label)
	{
		struct PredVisitor {
			Generator& gen;
			const std::string& end_label;

			void operator()(const NodeIfPredElif* elif) const
			{
				gen.gen_expr(elif->expr);
				gen.pop("eax");
				const std::string label = gen.create_label();
				gen.m_output << "	test eax, eax\n";
				gen.m_output << "	jz " << label << "\n";
				gen.gen_scope(elif->scope);
				gen.m_output << "	jmp " << end_label << "\n";
				gen.m_output << "	" << label << ":\n";
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

	void gen_stmt(const NodeStmt* stmt)
	{
		struct StmtVisitor {
			Generator& gen;

			void operator()(const NodeStmtExit* stmt_exit) const
			{
				DataType etype = gen.type_of_expr(stmt_exit->expr);
				if(etype != DataTypeInt) {
					gen.GeneratorError(stmt_exit->def, "`exit` except type `int`\nNOTE: but got type " + dt_to_string(etype));
				}
				gen.gen_expr(stmt_exit->expr);
				gen.m_output << "	call ExitProcess@4\n";
			}

			void operator()(const NodeStmtProc* stmt_proc)
			{
				std::optional<Procedure> proc = gen.proc_lookup(stmt_proc->name);
				if(proc.has_value()) {
					Token pdef = proc.value().def;
					gen.GeneratorError(stmt_proc->def, "procedure `" + stmt_proc->name + "` redefenition.\nNOTE: first defenition here " + loc_of(pdef) + ".");
				}
				size_t fsz = gen.collect_alligns(stmt_proc->scope);
				gen.m_procs.push_back({ .name = stmt_proc->name , .params = stmt_proc->params , .rettype = stmt_proc->rettype, .stack_allign = stmt_proc->params.size() + fsz, .attrs = stmt_proc->attrs , .def = stmt_proc->def });
				for(int i = 0;i < static_cast<int>(stmt_proc->params.size());++i) {
					gen.create_var_va(stmt_proc->params[i].first, stmt_proc->params[i].second, stmt_proc->def);
				}
				std::vector<ProcAttr> attrs = stmt_proc->attrs; 
				bool noprolog = std::find(attrs.begin(), attrs.end(), ProcAttr::noprolog) != attrs.end();
				gen.m_output << stmt_proc->name << ":\n";
				if(!noprolog) {
					gen.m_output << "	push ebp\n";
					gen.m_output << "	mov ebp, esp\n";
				}
				size_t scope_size = stmt_proc->params.size() + fsz;
				bool nostdargs = std::find(attrs.begin(), attrs.end(), ProcAttr::nostdargs) != attrs.end();
				if(noprolog && !nostdargs) {
					gen.GeneratorError(stmt_proc->def, "attribute noprolog without nostdargs\nNOTE: it should cause a error in runtime");
				}
				if(nostdargs) {
					scope_size -= stmt_proc->params.size();
				}
				if(scope_size != 0) {
					gen.m_output << "	sub esp, " << scope_size * 4 << "\n";
				}
				if(static_cast<int>(stmt_proc->params.size()) != 0U && !nostdargs) {
					int rev_i = 0;
					for(int i = static_cast<int>(stmt_proc->params.size()) - 1;i > -1;--i, rev_i++) {
						gen.m_output << "	mov edx, dword [ebp+" << rev_i * 4 + 8 << "]\n";
						gen.m_output << "	mov dword [ebp-" << rev_i * 4 + 4 << "], edx\n";
					}
				}
				gen.m_cur_proc = gen.m_procs[gen.m_procs.size() - 1];
				gen.gen_scope(stmt_proc->scope);
				gen.m_cur_proc = std::nullopt;
				if(stmt_proc->name == "main") {
					gen.m_output << "	xor eax, eax\n";
				}
				if(scope_size != 0) {
					gen.m_output << "	add esp, " << scope_size * 4 << "\n";
				}
				if(!noprolog) {
					gen.m_output << "	pop ebp\n";
				}
				gen.m_output << "	ret\n\n";
				gen.m_vars.clear();
				gen.m_var_index = 0U;
			}

			void operator()(const NodeStmtReturn* stmt_return) const
			{
				std::optional<Procedure> cproc = gen.m_cur_proc;
				if(!cproc.has_value()) {
					gen.GeneratorError(stmt_return->def, "return without procedure");
				}
				DataType rettype = cproc.value().rettype;
				if(rettype == DataTypeVoid) {
					gen.GeneratorError(stmt_return->def, "return from void procedure with value");
				}
				if(gen.type_of_expr(stmt_return->expr) != rettype) {
					gen.GeneratorError(stmt_return->def, "procedure `" + cproc.value().name + "` at return except type " + dt_to_string(rettype) + "\nNOTE: but got type " + dt_to_string(gen.type_of_expr(stmt_return->expr)));
				}
				gen.gen_expr(stmt_return->expr);
				gen.pop("eax");
				gen.end_scope_fsz(cproc.value().stack_allign);
				gen.m_output << "	pop ebp\n";
				gen.m_output << "	ret\n";
			}

			void operator()(const NodeStmtLet* stmt_let) const
			{
				gen.create_var(stmt_let->ident.value.value(), stmt_let->expr, stmt_let->ident);
			}

			void operator()(const NodeStmtLetNoAssign* stmt_let) const
			{
				gen.create_var_va(stmt_let->ident.value.value(), stmt_let->type, stmt_let->ident);
			}

			void operator()(const NodeStmtAssign* stmt_assign) const
			{
				NodeExpr* lvalue = stmt_assign->lvalue;
				DataType ltype = gen.type_of_expr(stmt_assign->lvalue);
				DataType vtype = gen.type_of_expr(stmt_assign->expr);
				if(ltype != vtype) {
					gen.GeneratorError(stmt_assign->def, "at = except type " + ltype.to_string() + "\nNOTE: but got " + vtype.to_string());
				}
				if(std::holds_alternative<NodeTerm*>(lvalue->var)) {
					NodeTerm* lvterm = std::get<NodeTerm*>(lvalue->var);
					if(std::holds_alternative<NodeTermIdent*>(lvterm->var)) {
						NodeTermIdent* lvident = std::get<NodeTermIdent*>(lvterm->var);
						std::string name = lvident->ident.value.value();
						std::optional<Var> var = gen.var_lookup(name);
						if(!var.has_value()) {
							gen.GeneratorError(stmt_assign->def, "unkown variable `" + name + "` at assignment");
						}
						gen.gen_expr(stmt_assign->expr);
						gen.pop("edx");
						gen.m_output << "	mov dword [ebp-" << var.value().stack_loc << "], edx\n";
						return;
					}
				}
				gen.gen_expr(stmt_assign->lvalue, true);
				gen.gen_expr(stmt_assign->expr);
				gen.pop("ecx");
				gen.pop("edx");
				gen.m_output << "	mov dword [edx], ecx\n";
			}

			void operator()(const NodeStmtCall* stmt_call) const
			{
				const std::string name = stmt_call->def.value.value();
				std::optional<Procedure> proc = gen.proc_lookup(name);
				if(!proc.has_value()) {
					gen.GeneratorError(stmt_call->def, "unkown procedure `" + name + "`");
				}
				size_t stack_allign = 0;
				if(stmt_call->args.has_value()) {
					if(proc.value().params.size() == 0) {
						gen.GeneratorError(stmt_call->def, "procedure `" + name + "` don't excepts any arguments");
					}
					bool nosizedargs = std::find(proc.value().attrs.begin(), proc.value().attrs.end(), ProcAttr::nosizedargs) != proc.value().attrs.end();
					NodeExpr* args = stmt_call->args.value();
					if(std::holds_alternative<NodeBinExpr*>(args->var)) {
						NodeBinExpr* cexpr = std::get<NodeBinExpr*>(args->var);
						if(std::holds_alternative<NodeBinExprArgs*>(cexpr->var)) {
							std::vector<NodeExpr*> pargs = get<NodeBinExprArgs*>(cexpr->var)->args;
							if(pargs.size() != proc.value().params.size() && !nosizedargs) {
								gen.GeneratorError(stmt_call->def, "procedure `" + name + "` excepts " + std::to_string(proc.value().params.size()) + " arguments\nNOTE: but got " + std::to_string(pargs.size()));
							}
							if(pargs.size() < proc.value().params.size() && nosizedargs) {
								gen.GeneratorError(stmt_call->def, "procedure `" + name + "` excepts minimum" + std::to_string(proc.value().params.size()) + " arguments\nNOTE: but got " + std::to_string(pargs.size()));
							}
							for(int i = 0;i < static_cast<int>(proc.value().params.size());++i) {
								if(gen.type_of_expr(pargs[i]) != proc.value().params[i].second) {
									gen.GeneratorError(stmt_call->def, "procedure `" + name + "`\nexcept type " + dt_to_string(proc.value().params[i].second) + " at " + std::to_string(i) + " argument\nNOTE: but found type " + dt_to_string(gen.type_of_expr(pargs[i])));
								}
							}
							stack_allign += pargs.size();
						}
					} else {
						if(gen.type_of_expr(args) != proc.value().params[0].second) {
							gen.GeneratorError(stmt_call->def, "procedure `" + name + "`\nexcept type " + dt_to_string(proc.value().params[0].second) + " at 0 argument\nNOTE: but found type " + dt_to_string(gen.type_of_expr(args)));
						}
						if(proc.value().params.size() != 1U && !nosizedargs) {
							gen.GeneratorError(stmt_call->def, "procedure `" + name + "` excepts " + std::to_string(proc.value().params.size()) + " arguments\nNOTE: but got 0");
						}
						stack_allign++;
					}
				} else {
					if(proc.value().params.size() != 0U) {
						gen.GeneratorError(stmt_call->def, "procedure `" + name + "` excepts " + std::to_string(proc.value().params.size()) + " args\nNOTE: but got 0");
					}
				}
				if(stmt_call->args.has_value()) {
					gen.gen_expr(stmt_call->args.value());
				}
				gen.m_output << "	call " << name << "\n";
				if(stack_allign != 0) {
					gen.m_output << "	add esp, " << stack_allign * 4 << "\n";
				}
			}

			void operator()(const NodeScope* scope) const
			{
				gen.gen_scope(scope);
			}

			void operator()(const NodeStmtIf* stmt_if) const
			{
				gen.gen_expr(stmt_if->expr);
				gen.pop("eax");
				const std::string label = gen.create_label();
				gen.m_output << "	test eax, eax\n";
				gen.m_output << "	jz " << label << "\n";
				gen.gen_scope(stmt_if->scope);
				if (stmt_if->pred.has_value()) {
					const std::string end_label = gen.create_label();
					gen.m_output << "	jmp " << end_label << "\n";
					gen.m_output << "	" << label << ":\n";
					gen.gen_if_pred(stmt_if->pred.value(), end_label);
					gen.m_output << "	" << end_label << ":\n";
				}
				else {
					gen.m_output << "	" << label << ":\n";
				}
			}

			void operator()(const NodeStmtWhile* stmt_while) const
			{
				auto preiflab = gen.create_label();
				auto blocklab = gen.create_label();
				auto breaklab = gen.create_label();
				gen.m_output << "	" << preiflab << ":\n";
				gen.gen_expr(stmt_while->expr);
				gen.m_output << "	pop eax\n";
				gen.m_output << "	test eax, eax\n";
				gen.m_output << "	jz " << breaklab << "\n";
				gen.m_output << "	" << blocklab << ":\n";
				gen.gen_scope(stmt_while->scope);
				gen.m_output << "	jmp " << preiflab << "\n";
				gen.m_output << "	" << breaklab << ":\n";
			}

			void operator()(const NodeStmtStore* stmt_store) const
			{
				DataType ptype = gen.type_of_expr(stmt_store->ptr);
				DataType etype = gen.type_of_expr(stmt_store->expr);
				if(ptype != DataTypePtr && etype != DataTypeInt) {
					gen.GeneratorError(stmt_store->def, "store types missmatch\nNOTE: except `ptr`, `int`\nNOTE: but got " + dt_to_string(ptype) + ", " + dt_to_string(etype));
				}
				if(stmt_store->size == 8U) {
					gen.gen_expr(stmt_store->ptr);
					gen.gen_expr(stmt_store->expr);
					gen.m_output << "	pop edx\n";
					gen.m_output << "	pop ecx\n";
					gen.m_output << "	mov byte [ecx], dl\n";
				}
				else if(stmt_store->size == 16U) {
					gen.gen_expr(stmt_store->ptr);
					gen.gen_expr(stmt_store->expr);
					gen.m_output << "	pop edx\n";
					gen.m_output << "	pop ecx\n";
					gen.m_output << "	mov word [ecx], dx\n";
				}
				else if(stmt_store->size == 32U) {
					gen.gen_expr(stmt_store->ptr);
					gen.gen_expr(stmt_store->expr);
					gen.m_output << "	pop edx\n";
					gen.m_output << "	pop ecx\n";
					gen.m_output << "	mov dword [ecx], edx\n";
				} else {
					assert(false); // unreacheable
				}
			}

			void operator()(const NodeStmtBuffer* stmt_buf) {
				if(stmt_buf->size % 2 != 0) {
					gen.GeneratorError(stmt_buf->def, "size of buffer must be a even number");
				}
				gen.m_var_index += (stmt_buf->size / 4);
				gen.create_var_va_wid(stmt_buf->name, DataTypePtr, stmt_buf->def);
			}

			void operator()(const NodeStmtAsm* stmt_asm) {
				gen.m_output << "	" << stmt_asm->code << "\n";
			}

			void operator()(const NodeStmtCextern* stmt_cextern) {
				if(std::find(gen.m_cexterns.begin(), gen.m_cexterns.end(), stmt_cextern->name) != gen.m_cexterns.end()) {
					return; // already in externs
				}
				gen.m_cexterns.push_back(stmt_cextern->name);
			}

			void operator()(const NodeStmtStruct* stmt_struct) {
				gen.m_structs.push_back({ .name = stmt_struct->name, .fields = stmt_struct->fields });
			}

			void operator()(const NodeStmtDelete* stmt_delete) {
				bool is_object_expr = gen.type_of_expr(stmt_delete->expr).is_object;
				if(!is_object_expr) {
					gen.GeneratorError(stmt_delete->def, "`delete` except object\nNOTE: but got " + gen.type_of_expr(stmt_delete->expr).to_string());
				}
				gen.gen_expr(stmt_delete->expr);
				gen.m_output << "	call free\n";
				gen.m_output << "	add esp, 4\n";
			}
		};

		StmtVisitor visitor { .gen = *this };
		std::visit(visitor, stmt->var);
	}

	[[nodiscard]] std::string gen_prog()
	{
		std::stringstream result;
		result << "section .text\n\n";
		result << "global main\n\n";

		for (const NodeStmt* stmt : m_prog.stmts) {
			gen_stmt(stmt);
		}

		yforeach(m_cexterns) {
			result << "extern " << m_cexterns[i] << "\n";
		}

		result << "\n";

		m_output << "\nsection .data\n";
		m_output << "	numfmt: db \"%d\", 0x0\n";
		m_output << "	numfmtnl: db \"%d\", 0xa, 0x0\n";
		m_output << "	strfmt: db \"%s\", 0x0\n";
		for(int i = 0;i < static_cast<int>(m_strings.size());++i) {
			String& cur_s = m_strings[i];
			m_output << "	s_" << static_cast<int>(cur_s.index) << ": db ";
			std::stringstream hexstr;
			for(int j = 0;j < static_cast<int>(cur_s.value.length());++j) {
				hexstr << "0x" << std::hex << static_cast<int>(cur_s.value[j]) << ", ";
			}
			m_output << hexstr.str();
			hexstr.clear();
			m_output << "0x0\n";
		}
		m_output << "\nsection .bss\n";
		m_output << "	tmp_stor: resd 1\n";
		result << m_output.str();
		return result.str();
	}

private:
	void push(const std::string& reg)
	{
		m_output << "	push " << reg << "\n";
	}

	void pop(const std::string& reg)
	{
		m_output << "	pop " << reg << "\n";
	}

	void begin_scope() {}

	void end_scope() {}

	void begin_scope_fsz(int fsz)
	{
		if(fsz != 0) {
			m_output << "	sub esp, " << fsz * 4 << "\n";
		}
		m_scopes.push_back(m_vars.size());
	}

	void end_scope_fsz(int fsz)
	{
		if(fsz != 0) {
			m_output << "	add esp, " << fsz * 4 << "\n";
		}
		m_scopes.pop_back();
	}

	std::string create_label()
	{
		std::stringstream ss;
		ss << "L" << m_label_count++;
		return ss.str();
	}

	const NodeProg m_prog;
	std::stringstream		m_output   {};
	std::vector<Var>		 m_vars	 {};
	std::vector<String>	  m_strings  {};
	std::vector<size_t>	  m_scopes   {};
	std::vector<Procedure>   m_procs	{};
	std::vector<Struct> m_structs	   {};
	std::optional<Procedure> m_cur_proc {};
	std::vector<std::string> m_cexterns = {
		"ExitProcess@4",
		"malloc",
		"free"
	};
	size_t m_var_index = 0U;
	size_t m_label_count = 0U;
};