#pragma once

#define yforeach(container) for(int i = 0;i < static_cast<int>(container.size());++i)

enum class ProcAttr {
	nostdargs,
	noprolog,
	nosizedargs,
	cimport,
};

struct InterfaceMethod {
    Token def;
    GString name;
    GVector<std::pair<GString, DataType>> params;
    DataType rettype;
};

std::optional<ProcAttr> string_to_PA(const GString& str) {
	if(str == "nostdargs") {
		return ProcAttr::nostdargs;
	}
	if(str == "noprolog") {
		return ProcAttr::noprolog;
	}
	if(str == "nosizedargs") {
		return ProcAttr::nosizedargs;
	}
	if(str == "cimport") {
		return ProcAttr::cimport;
	}
	return std::nullopt;
}

bool file_exists(const GString& name) {
	if(FILE *file = fopen(name.c_str(), "r")) {
		fclose(file);
		return true;
	} else {
		return false;
	}
}

namespace ptools {
	namespace as {
		NodeTerm* term(NodeExpr* expr) {
			return std::get<NodeTerm*>(expr->var);
		}
		NodeTermIdent* ident(NodeExpr* expr) {
			return std::get<NodeTermIdent*>(std::get<NodeTerm*>(expr->var)->var);
		}
		NodeTermType* type(NodeExpr* expr) {
			return std::get<NodeTermType*>(std::get<NodeTerm*>(expr->var)->var);
		}
	}
	namespace is {
		bool ident(NodeExpr* expr) {
			if(!std::holds_alternative<NodeTerm*>(expr->var)) {
				return false;
			}
			return std::holds_alternative<NodeTermIdent*>(std::get<NodeTerm*>(expr->var)->var);
		}
		bool type(NodeExpr* expr) {
			if(!std::holds_alternative<NodeTerm*>(expr->var)) {
				return false;
			}
			return std::holds_alternative<NodeTermType*>(std::get<NodeTerm*>(expr->var)->var);
		}
		bool term(NodeExpr* expr) {
			return std::holds_alternative<NodeTerm*>(expr->var);
		}
	}
	namespace get {
		std::optional<NodeTermIdent*> ident(NodeExpr* expr) {
			if(!is::ident(expr)) return std::nullopt;
			return as::ident(expr);
		}
		std::optional<NodeTermType*> type(NodeExpr* expr) {
			if(!is::type(expr)) return std::nullopt;
			return as::type(expr);
		}
		std::optional<NodeTerm*> term(NodeExpr* expr) {
			if(!is::term(expr)) return std::nullopt;
			return as::term(expr);
		}
	}
}

struct Constant {
	GString name;
	int value;
};

struct Macro {
	GString name;
	GVector<GString> args;
	GVector<Token> body;
	bool is_pure;
};

struct TokenStream {
    GVector<Token> tokens;
    size_t index = 0;
};

class TokenStreamCursor {
public:
    TokenStreamCursor() = default;

    void init_main(const GVector<Token>& toks) {
        m_stack.clear();
        TokenStream s;
        s.tokens = toks; 
        s.index  = 0;
        m_stack.push_back(std::move(s));
    }

    void push_stream(GVector<Token>&& toks) {
        TokenStream s;
        s.tokens = std::move(toks);
        s.index  = 0;
        m_stack.push_back(std::move(s));
    }

    bool eof() const {
        for (int i = static_cast<int>(m_stack.size()) - 1; i >= 0; --i) {
            const auto& s = m_stack[static_cast<size_t>(i)];
            if (s.index < s.tokens.size())
                return false;
        }
        return true;
    }

    std::optional<Token> peek(int offset = 0) const {
        if (m_stack.empty()) return std::nullopt;

        int off = offset;
        for (int i = static_cast<int>(m_stack.size()) - 1; i >= 0; --i) {
            const auto& s = m_stack[static_cast<size_t>(i)];
            size_t avail = s.tokens.size() - s.index;
            if (off < static_cast<int>(avail)) {
                return s.tokens[s.index + static_cast<size_t>(off)];
            }
            off -= static_cast<int>(avail);
        }
        return std::nullopt;
    }

    std::optional<Token> consume_opt() {
        while (!m_stack.empty()) {
            auto& s = m_stack.back();
            if (s.index < s.tokens.size()) {
                return s.tokens[s.index++];
            }
            m_stack.pop_back();
        }
        return std::nullopt;
    }

    Token consume() {
        auto t = consume_opt();
        if (!t.has_value()) {
            Token fake;
            fake.type = TokenType_t::ident;
            fake.line = 0;
            fake.col  = 0;
            fake.file = "<eof>";
            return fake;
        }
        return t.value();
    }

    std::optional<Token> try_consume(TokenType_t tt) {
        auto t = peek();
        if (t.has_value() && t->type == tt) {
            return consume();
        }
        return std::nullopt;
    }

    void split_shift_right_token() {
        if (m_stack.empty()) return;
        TokenStream& s = m_stack.back();
        if (s.index >= s.tokens.size()) return;

        Token& tok = s.tokens[s.index];
        if (tok.type != TokenType_t::shift_right) return;

        tok.type  = TokenType_t::above;
        tok.value = ">";

        Token next_tok = tok;
        next_tok.col += 1;
        s.tokens.insert(s.tokens.begin() + static_cast<std::ptrdiff_t>(s.index + 1),
                        next_tok);
    }

private:
    GVector<TokenStream> m_stack;
};

class Parser {
public:

	explicit Parser(const GVector<Token>& tokens, DiagnosticManager* _dman, ArenaAllocator* arenalloc)
    {
        m_tok_cursor.init_main(tokens);
        m_diag_man = _dman;
        m_allocator = arenalloc;
    }

	std::optional<Macro> macro_lookup(const GString& name) const noexcept {
		const auto& search = m_macroses.find(name);
		if(search != m_macroses.end()) {
			return search->second;
		}
		return std::nullopt;
	}

	DataType parse_type() {
	    Token __base = consume();
	    if(!is_type_token(__base.type)) {
	        ParsingError("excepted type");
	    }
	    
	    BaseDataType type_base = uni_token_to_dt(__base);
	    DataType res(type_base);

	    if(peek().has_value() && peek().value().type == TokenType_t::less) {
	        consume();
	        while(peek().has_value() && peek().value().type != TokenType_t::above) {
	            check_split_shr(); 
	            if(peek().value().type == TokenType_t::above) break;

	            DataType arg_type = parse_type();
	            res.node->generics.push_back(arg_type);

	            check_split_shr();
	            if(peek().has_value() && peek().value().type != TokenType_t::above) {
	                try_consume_err(TokenType_t::comma);
	            }
	        }
	        consume();
	    }
	    while(peek().has_value() && peek().value().type == TokenType_t::star) {
	        consume();
	        res.root().ptrlvl++;
	    }
	    if(peek().has_value() && peek().value().type == TokenType_t::double_ampersand) {
	        res.root().rvalue = true;
	        consume();
	    }
	    else if(peek().has_value() && peek().value().type == TokenType_t::ampersand) {
	        res.root().link = true;
	        consume();
	    }
	    return res;
	}

	void ParsingError(const GString& msg, const int pos = 0) noexcept
	{
	    auto tok_opt = peek(pos);
	    if (tok_opt.has_value()) {
	        m_diag_man->DiagnosticMessage(tok_opt.value(), "error", msg, 0);
	    } else if (peek().has_value()) {
	        m_diag_man->DiagnosticMessage(peek().value(), "error", msg, 0);
	    } else {
	        Token fake;
	        fake.type = TokenType_t::ident;
	        fake.line = 0;
	        fake.col = 0;
	        fake.file = "<eof>";
	        m_diag_man->DiagnosticMessage(fake, "error", msg, 0);
	    }
	    exit(EXIT_FAILURE);
	}

	void ParsingError_t(const GString& msg, const Token& tok, bool ex = true) noexcept
	{
		m_diag_man->DiagnosticMessage(tok, "error", msg, 0);
		if(ex) exit(EXIT_FAILURE);
	}

	void ParsingNote(const GString& msg, const Token& tok, bool ex = true) noexcept
	{
		m_diag_man->DiagnosticMessage(tok, "note", msg, 0);
		if(ex) exit(EXIT_FAILURE);
	}

	void error_expected(const GString& msg)
	{
		if(peek().has_value()) {
			ParsingError("excepted " + msg + " but got " + tok_to_string(peek(0).value().type), -1);
		} else {
			ParsingError("excepted " + msg + " but got nothing", -1);
		}
		exit(EXIT_FAILURE);
	}

	GVector<NodeExpr*> __getargs(const NodeExpr* __expr) {
		return std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(__expr->var)->var)->args;
	}

	std::pair<NodeExpr*, size_t> parse_args(NodeExpr* dop = NULL) {
		NodeBinExprArgs* args = m_allocator->emplace<NodeBinExprArgs>();
		if(dop != NULL) args->args.push_back(dop);
		size_t size = 0;
		while(peek().has_value() && (peek().value().type != TokenType_t::close_paren && peek().value().type != TokenType_t::semi)) {
			auto expr = parse_expr();
			if(!expr.has_value()) {
				error_expected("expression");
			}
			if(peek().has_value() && (peek().value().type != TokenType_t::comma && peek().value().type != TokenType_t::semi && peek().value().type != TokenType_t::close_paren)) {
				error_expected(",");
			}
			if(peek().value().type == TokenType_t::close_paren) {
				args->args.push_back(expr.value());
				size += 1;
				break;
			}
			consume();
			args->args.push_back(expr.value());
			size += 1;
		}
		NodeBinExpr* bexpr = m_allocator->emplace<NodeBinExpr>();
		bexpr->var = args;
		NodeExpr* fexpr = m_allocator->emplace<NodeExpr>();
		fexpr->var = bexpr;
		std::pair<NodeExpr*, size_t> res = std::make_pair(fexpr, size);
		return res;
	}

	GVector<GVector<Token>*>* parse_macro_args() {
		GVector<GVector<Token>*>* __args = m_allocator->alloc<GVector<GVector<Token>*>>();
		__args->push_back(m_allocator->alloc<GVector<Token>>());
		size_t nest_lvl = 0ULL;
		while(true) {
			if(peek().value().type == TokenType_t::open_paren) {
				nest_lvl += 1ULL;
				if(nest_lvl == 1) {
					consume();
					continue;
				}
			}
			else if(peek().value().type == TokenType_t::close_paren) {
				nest_lvl -= 1ULL;
				if(nest_lvl == 0) {
					break;
				}
			}
			Token cp = consume();
			if(cp.type == TokenType_t::comma && nest_lvl == 1) {
				__args->push_back(m_allocator->alloc<GVector<Token>>());
			} else {
				__args->operator[](__args->size() - 1)->push_back(cp);
			}
		}
		consume();
		return __args;
	}

	void print_macro_args(GVector<GVector<Token>*>* __args) {
		for(int i = 0;i < static_cast<int>(__args->size());++i) {
			std::cout << "-------------\n";
			GVector<Token>* L_args = __args->operator[](i);
			for(int j = 0;j < static_cast<int>(L_args->size());++j) {
				std::cout << L_args->operator[](j) << std::endl;
			}
			std::cout << "-------------\n";
		}
	}

	std::optional<size_t> __macro_arg_pos(Macro& __macro, const GString& __arg) {
		for(int i = 0;i < static_cast<int>(__macro.args.size());++i) {
			if(__macro.args[i] == __arg) {
				return i;
			}
		}
		return std::nullopt;
	}

	void expand_macro(Macro& _macro,
                          GVector<GVector<Token>*>* __args,
                          Token& __at)
	{
	    GVector<Token> expanded;
	    expanded.reserve(_macro.body.size() + 8);

	    for (Token tok : _macro.body) {
	        tok.line = __at.line;
	        tok.col  = __at.col;
	        tok.file = __at.file;

	        if (__at.expand.has_value()) {
	            if (tok.expand.has_value())
	                __at.expand.value()->expand = tok.expand;
	            tok.expand = __at.expand;
	        }

	        if (tok.type == TokenType_t::ident && tok.value.has_value()) {
	            if (auto pos = __macro_arg_pos(_macro, tok.value.value())) {
	                auto* arg_tokens = __args->operator[](*pos);
	                expanded.insert(expanded.end(),
	                                arg_tokens->begin(),
	                                arg_tokens->end());
	                continue;
	            }
	        }
	        expanded.push_back(tok);
	    }

	    m_tok_cursor.push_stream(std::move(expanded));
	}

	void __expand_str(NodeTermStrLit* str) {
		while(peek().has_value() && peek().value().type == TokenType_t::string_lit) {
			str->str_lit.operator+=(consume().value.value());
		}
	}

	void check_split_shr() {
        auto t = peek();
        if (t.has_value() && t->type == TokenType_t::shift_right) {
            m_tok_cursor.split_shift_right_token();
        }
    }

	bool is_temp_call() {
		if(!peek().has_value()) return false;
		if(peek().value().type != TokenType_t::ident) return false;
		
		if(!peek(1).has_value()) return false;
		if(peek(1).value().type != TokenType_t::less) return false;

		int i = 2;
		int nest = 1;
		while(peek(i).has_value()) {
			TokenType_t t = peek(i).value().type;
			if(t == TokenType_t::less) {
				nest++;
			}
			else if(t == TokenType_t::above) {
				nest--;
				if(nest == 0) {
					i++;
					break;
				}
			}
			else if(t == TokenType_t::shift_right) {
				nest -= 2;
				if(nest <= 0) {
					if(nest < 0) return false;
					i++;
					break;
				}
			}
			else {
				if(!is_type_token(t) && 
				   t != TokenType_t::comma && 
				   t != TokenType_t::star && 
				   t != TokenType_t::double_ampersand && 
				   t != TokenType_t::ampersand &&
				   t != TokenType_t::double_colon) { 
					return false;
				}
			}
			i++;
		}
		
		if(nest != 0) return false;

		if(!peek(i).has_value()) return false;
		if(peek(i).value().type != TokenType_t::open_paren) return false;
		return true;
	}

	std::optional<NodeTerm*> parse_term(bool cmethod = false) // NOLINT(*-no-recursion)
	{
		if(auto int_lit = try_consume(TokenType_t::int_lit)) {
			auto term_int_lit = m_allocator->emplace<NodeTermIntLit>(int_lit.value());
			auto term = m_allocator->emplace<NodeTerm>(term_int_lit);
			return term;
		}
		if(auto _ct_eval = try_consume(TokenType_t::ct_eval)) {
			auto term_eval = m_allocator->emplace<NodeTermCtEval>();
			term_eval->def = _ct_eval.value();
			try_consume_err(TokenType_t::open_paren);
			if(auto _expr = parse_expr()) {
				term_eval->expr = _expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			auto term = m_allocator->emplace<NodeTerm>(term_eval);
			return term;
		}
		if (!cmethod && peek(1).has_value() && peek(1).value().type == TokenType_t::tilda) {
    		NodeExpr* objv;
    		if (auto _expr = parse_term(true)) {
    		    NodeTerm* asterm = _expr.value();
    		    objv = m_allocator->emplace<NodeExpr>();
    		    objv->var = asterm;
    		} else {
    		    error_expected("expression");
    		}
		
    		NodeTerm* last_term = nullptr;
		
    		while (peek().has_value() && peek().value().type == TokenType_t::tilda) {
    		    consume();
    		    Token identif = try_consume_err(TokenType_t::ident);
		
    		    auto term_call = m_allocator->emplace<NodeTermMtCall>();
    		    term_call->def  = identif;
    		    term_call->mt   = objv;
    		    term_call->name = identif.value.value();
		
    		    if (peek().has_value() && peek().value().type == TokenType_t::less) {
    		        consume();
    		        while (peek().has_value() && peek().value().type != TokenType_t::above) {
    		            DataType dt = parse_type();
    		            term_call->targs.push_back(dt);
    		            if (peek().has_value() && peek().value().type != TokenType_t::above) {
    		                try_consume_err(TokenType_t::comma);
    		            }
    		        }
    		        try_consume_err(TokenType_t::above);
    		    }
		
    		    try_consume_err(TokenType_t::open_paren);
    		    if (peek().has_value() && peek().value().type == TokenType_t::close_paren) {
    		        auto* _args = m_allocator->emplace<NodeBinExprArgs>();
    		        _args->args.push_back(objv);
    		        auto* ab = m_allocator->emplace<NodeBinExpr>();
    		        ab->var = _args;
    		        auto* ex = m_allocator->emplace<NodeExpr>();
    		        ex->var = ab;
    		        term_call->args = ex;
    		    } else {
    		        std::pair<NodeExpr*, size_t> pargs = parse_args(objv);
    		        term_call->args = pargs.first;
    		    }
    		    try_consume_err(TokenType_t::close_paren);
		
    		    last_term = m_allocator->emplace<NodeTerm>(term_call);
    		    objv = m_allocator->emplace<NodeExpr>();
    		    objv->var = last_term;
    		}
		
    		return last_term;
		}
		if(auto _sr = try_consume(TokenType_t::star)) {
			NodeTerm* trm;
			if(auto _term = parse_term()) {
				trm = _term.value();
			} else {
				error_expected("primary-expression");
			}
			NodeExpr* asx = m_allocator->emplace<NodeExpr>();
			asx->var = trm;
			auto unref_term = m_allocator->emplace<NodeTermUnref>();
			unref_term->def = _sr.value();
			unref_term->expr = asx;
			auto term = m_allocator->emplace<NodeTerm>(unref_term);
			return term;
		}
		if(auto _line = try_consume(TokenType_t::_line)) {
			auto line_term = m_allocator->emplace<NodeTermLine>(_line.value());
			auto term = m_allocator->emplace<NodeTerm>(line_term);
			return term;
		}
		if(auto _stexpr = try_consume(TokenType_t::expr_stmt)) {
			auto stmt_term = m_allocator->emplace<NodeTermExprStmt>();
			stmt_term->def = _stexpr.value();
			if(auto _scope = parse_scope()) {
				stmt_term->scope = _scope.value();
			} else {
				error_expected("scope");
			}
			try_consume_err(TokenType_t::open_paren);
			if(auto _expr = parse_expr()) {
				stmt_term->expr = _expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			auto term = m_allocator->emplace<NodeTerm>(stmt_term);
			return term;
		}
		if(auto _pop = try_consume(TokenType_t::popfromstack)) {
			auto pop_stmt = m_allocator->emplace<NodeTermPop>();
			try_consume_err(TokenType_t::open_paren);
			try_consume_err(TokenType_t::close_paren);
			auto term = m_allocator->emplace<NodeTerm>(pop_stmt);
			return term;
		}
		if(auto _type = try_consume(TokenType_t::_type)) {
			auto type_term = m_allocator->emplace<NodeTermType>();
			type_term->def = _type.value();
			try_consume_err(TokenType_t::open_paren);
			type_term->type = parse_type();
			try_consume_err(TokenType_t::close_paren);
			auto term = m_allocator->emplace<NodeTerm>(type_term);
			return term;
		}
		if(auto _mdefined = try_consume(TokenType_t::ct_mdefined)) {
			auto term_mdef = m_allocator->emplace<NodeTermCtMdefined>();
			try_consume_err(TokenType_t::open_paren);
			term_mdef->value = (m_macroses.find(try_consume_err(TokenType_t::ident).value.value()) != m_macroses.end());
			try_consume_err(TokenType_t::close_paren);
			auto term = m_allocator->emplace<NodeTerm>(term_mdef);
			return term;
		}
		if(auto _col = try_consume(TokenType_t::_col)) {
			auto col_term = m_allocator->emplace<NodeTermCol>(_col.value());
			auto term = m_allocator->emplace<NodeTerm>(col_term);
			return term;
		}
		if(auto _file = try_consume(TokenType_t::_file)) {
			auto file_term = m_allocator->emplace<NodeTermFile>(_file.value());
			auto term = m_allocator->emplace<NodeTerm>(file_term);
			return term;
		}
		if(auto ampersand = try_consume(TokenType_t::ampersand)) {
			auto term_amp = m_allocator->emplace<NodeTermAmpersand>();
			term_amp->def = ampersand.value();
			if(auto term = parse_term()) {
				auto expr = m_allocator->emplace<NodeExpr>();
				expr->var = term.value();
				term_amp->expr = expr;
			} else {
				error_expected("expression");
			}
			auto term = m_allocator->emplace<NodeTerm>(term_amp);
			return term;
		}
		if (auto str_lit = try_consume(TokenType_t::string_lit)) {
			auto term_str_lit = m_allocator->emplace<NodeTermStrLit>(str_lit.value().value.value());
			__expand_str(term_str_lit);
			auto term = m_allocator->emplace<NodeTerm>(term_str_lit);
			return term;
		}
		if (auto rd8 = try_consume(TokenType_t::read8)) {
			auto term_rd8 = m_allocator->emplace<NodeTermRd>();
			try_consume_err(TokenType_t::open_paren);
			term_rd8->def = rd8.value();
			term_rd8->size = 8U;
			if(auto expr = parse_expr()) {
				term_rd8->expr = expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			auto term = m_allocator->emplace<NodeTerm>(term_rd8);
			return term;
		}
		if (auto cast = try_consume(TokenType_t::cast)) {
			auto term_cast = m_allocator->emplace<NodeTermCast>();
			try_consume_err(TokenType_t::open_paren);
			term_cast->def = cast.value();
			term_cast->type = parse_type();
			try_consume_err(TokenType_t::comma);
			if(auto expr = parse_expr()) {
				term_cast->expr = expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			auto term = m_allocator->emplace<NodeTerm>(term_cast);
			return term;
		}
		if (auto cast_to = try_consume(TokenType_t::cast_to)) {
			auto term_cast_to = m_allocator->emplace<NodeTermCastTo>();
			try_consume_err(TokenType_t::open_paren);
			term_cast_to->def = cast_to.value();
			if(auto _expr = parse_expr()) {
				term_cast_to->to = _expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::comma);
			if(auto expr = parse_expr()) {
				term_cast_to->expr = expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			auto term = m_allocator->emplace<NodeTerm>(term_cast_to);
			return term;
		}
		if (auto rd16 = try_consume(TokenType_t::read16)) {
			auto term_rd16 = m_allocator->emplace<NodeTermRd>();
			term_rd16->def = rd16.value();
			term_rd16->size = 16U;
			try_consume_err(TokenType_t::open_paren);
			if(auto expr = parse_expr()) {
				term_rd16->expr = expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			auto term = m_allocator->emplace<NodeTerm>(term_rd16);
			return term;
		}
		if (auto rd32 = try_consume(TokenType_t::read32)) {
			auto term_rd32 = m_allocator->emplace<NodeTermRd>();
			term_rd32->def = rd32.value();
			try_consume_err(TokenType_t::open_paren);
			term_rd32->size = 32U;
			if(auto expr = parse_expr()) {
				term_rd32->expr = expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			auto term = m_allocator->emplace<NodeTerm>(term_rd32);
			return term;
		}
		if (auto drval = try_consume(TokenType_t::__drvalue)) {
			auto term_drval = m_allocator->emplace<NodeTermDrvalue>();
			term_drval->def = drval.value();
			try_consume_err(TokenType_t::open_paren);
			if(auto expr = parse_expr()) {
				term_drval->expr = expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			auto term = m_allocator->emplace<NodeTerm>(term_drval);
			return term;
		}
		if((peek().has_value() && peek().value().type == TokenType_t::ident &&
			peek(1).has_value() && peek(1).value().type == TokenType_t::open_paren)
			|| (is_temp_call())
			) {
			Token identif = try_consume_err(TokenType_t::ident);
			std::optional<Macro> __macro = macro_lookup(identif.value.value());
			if(__macro.has_value()) {
				if(__macro.value().is_pure) ParsingError("can't expand pure macro `" + __macro.value().name + "`");
				GVector<GVector<Token>*>* __args = parse_macro_args();
				Macro _macro = __macro.value();
				expand_macro(_macro, __args, identif);
				return parse_term();
			}
			auto expr_call = m_allocator->emplace<NodeTermCall>();
			expr_call->def = identif;
			expr_call->name = identif.value.value();
			if(peek().has_value() && peek().value().type == TokenType_t::less) {
				consume();
				while(peek().has_value() && peek().value().type != TokenType_t::above) {
					DataType dt = parse_type();
					expr_call->targs.push_back(dt);
					if(peek().has_value() && peek().value().type != TokenType_t::above) {
						try_consume_err(TokenType_t::comma);
					}
				}
				consume();
			}
			try_consume_err(TokenType_t::open_paren);
			if(peek().has_value() && peek().value().type == TokenType_t::close_paren) {
				expr_call->args = std::nullopt;
			} else {
				std::pair<NodeExpr*, size_t> pargs = parse_args();
				expr_call->args = pargs.first;
			}
			try_consume_err(TokenType_t::close_paren);
			auto stmt = m_allocator->emplace<NodeTerm>(expr_call);
			return stmt;
		}
	
        if (peek().has_value() &&
            peek().value().type == TokenType_t::ident &&
            peek(1).has_value() &&
            peek(1).value().type == TokenType_t::double_colon)
        {
            Token first = try_consume_err(TokenType_t::ident);
            GVector<GString> segments;
            segments.push_back(first.value.value());

            while (peek().has_value() && peek().value().type == TokenType_t::double_colon) {
                consume();
                Token segTok = try_consume_err(TokenType_t::ident);
                segments.push_back(segTok.value.value());
            }

            if (segments.size() < 2) {
                ParsingError("invalid qualified name");
            }

            GString funcName = segments.back();
            segments.pop_back();

            GString nsName;
            for (size_t i = 0; i < segments.size(); ++i) {
                if (i) nsName += "::";
                nsName += segments[i];
            }

            if (peek().has_value() && 
               (peek().value().type == TokenType_t::less || peek().value().type == TokenType_t::open_paren))
            {
                auto expr_call = m_allocator->emplace<NodeTermNmCall>();
                expr_call->def  = first;
                expr_call->nm   = segments;  
                expr_call->name = funcName;

                if (peek().has_value() && peek().value().type == TokenType_t::less) {
                    consume();
                    while (peek().has_value() && peek().value().type != TokenType_t::above) {
                        DataType dt = parse_type();
                        expr_call->targs.push_back(dt);
                        if (peek().has_value() && peek().value().type != TokenType_t::above) {
                            try_consume_err(TokenType_t::comma);
                        }
                    }
                    try_consume_err(TokenType_t::above);
                }

                try_consume_err(TokenType_t::open_paren);
                if (peek().has_value() && peek().value().type == TokenType_t::close_paren) {
                    expr_call->args = std::nullopt;
                } else {
                    std::pair<NodeExpr*, size_t> pargs = parse_args();
                    expr_call->args = pargs.first;
                }
                try_consume_err(TokenType_t::close_paren);

                auto stmt = m_allocator->emplace<NodeTerm>(expr_call);
                return stmt;
            } 
            else {
                auto term_nm = m_allocator->emplace<NodeTermNmIdent>();
                term_nm->def = first;
                term_nm->nm = segments;
                term_nm->name = funcName;
                
                auto term = m_allocator->emplace<NodeTerm>(term_nm);
                return term;
            }
        }
		if(auto ident = try_consume(TokenType_t::ident)) {
			auto expr_ident = m_allocator->emplace<NodeTermIdent>();
			expr_ident->ident = ident.value();
			auto term = m_allocator->emplace<NodeTerm>(expr_ident);
			return term;
		}
		if(auto _sizeof = try_consume(TokenType_t::_sizeof)) {
    		auto sizeof_term = m_allocator->emplace<NodeTermSizeof>();
    		sizeof_term->def = _sizeof.value();
    		try_consume_err(TokenType_t::open_paren);
		
    		if (peek().has_value() && is_type_token(peek().value().type)) {
    		    sizeof_term->type = parse_type();
    		} else {
    		    if (auto _expr = parse_expr()) {
    		        sizeof_term->expr = _expr;
    		    } else {
    		        error_expected("expression");
    		    }
    		}
		
    		try_consume_err(TokenType_t::close_paren);
    		auto term = m_allocator->emplace<NodeTerm>(sizeof_term);
    		return term;
		}
		if(auto _typeid = try_consume(TokenType_t::_typeid)) {
			auto typeid_term = m_allocator->emplace<NodeTermTypeid>();
			try_consume_err(TokenType_t::open_paren);
			typeid_term->def = _typeid.value();
			Token lookahead = peek().value();
			typeid_term->ptype = std::nullopt;
			if(is_type_token(lookahead.type)) {
				consume();
				typeid_term->ptype = uni_token_to_dt(lookahead);
			}
			else {
				if(auto _expr = parse_expr()) {
					typeid_term->expr = _expr.value();
				} else {
					error_expected("expression");
				}
			}
			try_consume_err(TokenType_t::close_paren);
			auto term = m_allocator->emplace<NodeTerm>(typeid_term);
			return term;
		}
		if(const auto open_paren = try_consume(TokenType_t::open_paren)) {
			auto expr = parse_expr();
			if (!expr.has_value()) {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			auto term_paren = m_allocator->emplace<NodeTermParen>(expr.value());
			auto term = m_allocator->emplace<NodeTerm>(term_paren);
			return term;
		}
		return std::nullopt;
	}

	std::optional<NodeExpr*> parse_expr(const int min_prec = 0, bool lcmethod = false) // NOLINT(*-no-recursion)
	{
		std::optional<NodeTerm*> term_lhs = parse_term(lcmethod);
		if (!term_lhs.has_value()) {
			return {};
		}
		auto expr_lhs = m_allocator->emplace<NodeExpr>(term_lhs.value());

		auto get_op_prec = [&](TokenType_t t) -> std::optional<int> {
			if (t == TokenType_t::dot) return 10000;
			return bin_prec(t);
		};

		while(true) {
			std::optional<Token> curr_tok = peek();
			std::optional<int> prec;
			if(curr_tok.has_value()) {
				prec = get_op_prec(curr_tok->type);
				if (!prec.has_value() || prec < min_prec) {
					break;
				}
			}
			else {
				break;
			}

			const auto [type, line, col, value, file, expanded] = consume();
			Token ctok = {type, line, col, value, file, expanded};
			
			const int next_min_prec = prec.value() + 1;
			
			auto expr_rhs = parse_expr(next_min_prec, lcmethod);
			
			if (!expr_rhs.has_value()) {
				error_expected("expression");
			}
			
			auto expr = m_allocator->emplace<NodeBinExpr>();
			auto expr_lhs2 = m_allocator->emplace<NodeExpr>();
			
			expr_lhs2->var = expr_lhs->var;

			if(type == TokenType_t::dot) {
				bool rotated = false;
				if(std::holds_alternative<NodeTerm*>(expr_rhs.value()->var)) {
					NodeTerm* rhs_term = std::get<NodeTerm*>(expr_rhs.value()->var);
					if(std::holds_alternative<NodeTermMtCall*>(rhs_term->var)) {
						NodeTermMtCall* call = std::get<NodeTermMtCall*>(rhs_term->var);
						
						auto dot_node = m_allocator->emplace<NodeBinExprDot>(expr_lhs2, call->mt);
					
						auto new_obj_expr = m_allocator->emplace<NodeExpr>();
						auto dot_bin_expr = m_allocator->emplace<NodeBinExpr>();
						dot_bin_expr->def = ctok; 
						dot_bin_expr->var = dot_node;
						new_obj_expr->var = dot_bin_expr;

						call->mt = new_obj_expr;

						if (call->args.has_value()) {
							NodeExpr* args_expr = call->args.value();
							if (std::holds_alternative<NodeBinExpr*>(args_expr->var)) {
								NodeBinExpr* args_bin = std::get<NodeBinExpr*>(args_expr->var);
								if (std::holds_alternative<NodeBinExprArgs*>(args_bin->var)) {
									NodeBinExprArgs* args_list = std::get<NodeBinExprArgs*>(args_bin->var);
									if (!args_list->args.empty()) {
										args_list->args[0] = new_obj_expr; 
									}
								}
							}
						}
						expr_lhs->var = rhs_term;
						
						rotated = true;
					}
				}

				if (!rotated) {
					auto dot = m_allocator->emplace<NodeBinExprDot>(expr_lhs2, expr_rhs.value());
					expr->def = ctok;
					expr->var = dot;
					expr_lhs->var = expr;
				}
				continue;
			}
			else if(type == TokenType_t::open_bracket) {
                if (!expr_rhs.has_value()) {
                	error_expected("expression");
                }
                try_consume_err(TokenType_t::close_bracket);
                auto idx_node = m_allocator->emplace<NodeBinExprIndex>();
                idx_node->lhs = expr_lhs2;
                idx_node->rhs = expr_rhs.value();
                expr->def = ctok;
                expr->var = idx_node;
                expr_lhs->var = expr;
                continue;
            }
			else if(type == TokenType_t::plus) {
				auto add = m_allocator->emplace<NodeBinExprAdd>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = add;
			}
			else if(type == TokenType_t::star) {
				auto multi = m_allocator->emplace<NodeBinExprMulti>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = multi;
			}
			else if(type == TokenType_t::minus) {
				auto sub = m_allocator->emplace<NodeBinExprSub>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = sub;
			}
			else if(type == TokenType_t::fslash) {
				auto div = m_allocator->emplace<NodeBinExprDiv>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = div;
			}
			else if(type == TokenType_t::mod) {
				auto md = m_allocator->emplace<NodeBinExprMod>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = md;
			}
			else if(type == TokenType_t::eqeq) {
				auto eqeq = m_allocator->emplace<NodeBinExprEqEq>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = eqeq;
			}
			else if(type == TokenType_t::_not_eq) {
				auto nq = m_allocator->emplace<NodeBinExprNotEq>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = nq;
			}
			else if(type == TokenType_t::less) {
				auto less = m_allocator->emplace<NodeBinExprLess>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = less;
			}
			else if(type == TokenType_t::above) {
				auto above = m_allocator->emplace<NodeBinExprAbove>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = above;
			}
			else if(type == TokenType_t::double_ampersand) {
				auto band = m_allocator->emplace<NodeBinExprAnd>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = band;
			}
			else if(type == TokenType_t::double_stick) {
				auto bor = m_allocator->emplace<NodeBinExprOr>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = bor;
			}
			else if(type == TokenType_t::shift_left) {
				auto shl = m_allocator->emplace<NodeBinExprShl>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = shl;
			}
			else if(type == TokenType_t::shift_right) {
				auto shr = m_allocator->emplace<NodeBinExprShr>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = shr;
			}
			else {
				assert(false);
			}
			
			expr_lhs->var = expr;
		}
		return expr_lhs;
	}
	
	std::optional<NodeScope*> parse_scope() // NOLINT(*-no-recursion)
	{
		if(!try_consume(TokenType_t::open_curly).has_value()) {
			return {};
		}
		auto scope = m_allocator->emplace<NodeScope>();
		while(true) {
			auto stmt = parse_stmt();
			if(m_preprocessor_stmt) {
				m_preprocessor_stmt = false;
			} else {
				if(!stmt.has_value()) {
					break;
				}
				scope->stmts.push_back(stmt.value());
			}
		}
		try_consume_err(TokenType_t::close_curly);
		return scope;
	}

	std::optional<NodeIfPred*> parse_if_pred() // NOLINT(*-no-recursion)
	{
		if(try_consume(TokenType_t::elif)) {
			try_consume_err(TokenType_t::open_paren);
			const auto elif = m_allocator->alloc<NodeIfPredElif>();
			if (const auto expr = parse_expr()) {
				elif->expr = expr.value();
			}
			else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			if(peek().has_value() && peek().value().type == TokenType_t::open_curly) {
				if (const auto scope = parse_scope()) {
					elif->scope = scope.value();
				}
				else {
					error_expected("scope");
				}
			} else {
				if(const auto _stmt = parse_stmt()) {
					NodeScope* _scope = m_allocator->emplace<NodeScope>();
					_scope->stmts.push_back(_stmt.value());
					elif->scope = _scope;
				} else {
					error_expected("statement");
				}
			}
			elif->pred = parse_if_pred();
			auto pred = m_allocator->emplace<NodeIfPred>(elif);
			return pred;
		}
		if (try_consume(TokenType_t::else_)) {
			auto else_ = m_allocator->alloc<NodeIfPredElse>();
			if(peek().has_value() && peek().value().type == TokenType_t::open_curly) {
				if (const auto scope = parse_scope()) {
					else_->scope = scope.value();
				}
				else {
					error_expected("scope");
				}
			} else {
				if(const auto _stmt = parse_stmt()) {
					NodeScope* _scope = m_allocator->emplace<NodeScope>();
					_scope->stmts.push_back(_stmt.value());
					else_->scope = _scope;
				} else {
					error_expected("statement");
				}
			}
			auto pred = m_allocator->emplace<NodeIfPred>(else_);
			return pred;
		}
		return {};
	}

	std::optional<NodeStmt*> parse_stmt(bool expect_semi = true) // NOLINT(*-no-recursion)
	{
		std::optional<Token> tok0 = peek();
		if (!tok0.has_value()) {
    		return {};
		}
		if (peek().has_value() && peek().value().type == TokenType_t::exit && peek(1).has_value()
			&& peek(1).value().type == TokenType_t::open_paren) {
			consume();
			Token def = consume();
			auto stmt_exit = m_allocator->emplace<NodeStmtExit>();
			if (const auto node_expr = parse_expr()) {
				stmt_exit->expr = node_expr.value();
			}
			else {
				error_expected("expression");
			}
			stmt_exit->def = def;
			try_consume_err(TokenType_t::close_paren);
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_exit;
			return stmt;
		}
		if(auto _let = try_consume(TokenType_t::let)) {
			Token name = try_consume_err(TokenType_t::ident);
			auto stmt_let = m_allocator->emplace<NodeStmtLet>();
			stmt_let->type = std::nullopt;
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_let;
			if(auto _ = try_consume(TokenType_t::double_dot)) {
				stmt_let->type = parse_type();
			}
			stmt_let->ident = name;
			if(peek().has_value() && peek().value().type == TokenType_t::semi) {
				stmt_let->expr = std::nullopt;
				if(!stmt_let->type.has_value()) error_expected("type of initializer");
				consume();
				return stmt;
			}
			try_consume_err(TokenType_t::eq);
			if (const auto expr = parse_expr()) {
				stmt_let->expr = expr.value();
			}
			else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::semi);
			return stmt;
		}
		if (peek().has_value() && peek().value().type == TokenType_t::open_curly) {
			if (auto scope = parse_scope()) {
				auto stmt = m_allocator->emplace<NodeStmt>(scope.value());
				return stmt;
			}
			error_expected("scope");
		}
		if (auto if_ = try_consume(TokenType_t::if_)) {
			try_consume_err(TokenType_t::open_paren);
			auto stmt_if = m_allocator->emplace<NodeStmtIf>();
			if (const auto expr = parse_expr()) {
				stmt_if->expr = expr.value();
			}
			else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			if(peek().has_value() && peek().value().type == TokenType_t::open_curly) {
				if (const auto scope = parse_scope()) {
					stmt_if->scope = scope.value();
				}
				else {
					ParsingError("after if except `{` and `}` after body", -1);
				}
			} else {
				if(const auto _stmt = parse_stmt()) {
					NodeScope* _scope = m_allocator->emplace<NodeScope>();
					_scope->stmts.push_back(_stmt.value());
					stmt_if->scope = _scope;
				} else {
					error_expected("statement");
				}
			}
			stmt_if->pred = parse_if_pred();
			auto stmt = m_allocator->emplace<NodeStmt>(stmt_if);
			return stmt;
		}
		if (auto while_ = try_consume(TokenType_t::wwhile)) {
			try_consume_err(TokenType_t::open_paren);
			auto stmt_while = m_allocator->emplace<NodeStmtWhile>();
			if (const auto expr = parse_expr()) {
				stmt_while->expr = expr.value();
			}
			else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			if (const auto scope = parse_scope()) {
				stmt_while->scope = scope.value();
			}
			else {
				ParsingError("after while except `{` and `}` after body", -1);
			}
			auto stmt = m_allocator->emplace<NodeStmt>(stmt_while);
			return stmt;
		}
		if(peek().has_value() && peek().value().type == TokenType_t::proc) {
		    consume();
		    auto stmt_proc = m_allocator->emplace<NodeStmtProc>();
		    Token identif = try_consume_err(TokenType_t::ident);
		    stmt_proc->templates = NULL;

		    if (peek().has_value() && peek().value().type == TokenType_t::less) {
		        consume();
		        stmt_proc->templates = m_allocator->emplace<GVector<GString>>();
		        while (peek().has_value() && peek().value().type != TokenType_t::above) {
		            Token nm = try_consume_err(TokenType_t::ident);
		            GString tname = nm.value.value();
		            stmt_proc->templates->push_back(tname);

		            if (peek().has_value() && peek().value().type == TokenType_t::double_dot) {
		                consume();
		                DataType ifaceType = parse_type();
                        
		                TypeConstraint c;
		                c.type_param = tname;
		                c.iface_type = ifaceType;
		                stmt_proc->constraints.push_back(c);
		            }

		            if (peek().has_value() && peek().value().type != TokenType_t::above) {
		                try_consume_err(TokenType_t::comma);
		            }
		        }
		        try_consume_err(TokenType_t::above);
		    }

		    try_consume_err(TokenType_t::open_paren);
			GVector<std::pair<GString, DataType>> pparams;
			if(peek().has_value() && peek().value().type != TokenType_t::close_paren) {
				for(int i = 0;peek().has_value() && peek().value().type != TokenType_t::close_paren;++i) {
					DataType argtype = parse_type();
					Token argid = try_consume_err(TokenType_t::ident);
					pparams.push_back(std::make_pair(argid.value.value(), argtype));
					if(peek().has_value() && peek().value().type != TokenType_t::close_paren) {
						try_consume_err(TokenType_t::comma);
					}
				}
			}
			try_consume_err(TokenType_t::close_paren);
			stmt_proc->rettype = BaseDataTypeVoid;
			if(peek().value().type == TokenType_t::arrow) {
				consume();
				stmt_proc->rettype = parse_type();
			}
			stmt_proc->name = identif.value.value();
			stmt_proc->params = pparams;
			stmt_proc->def = identif;
			if(auto open_b = try_consume(TokenType_t::open_bracket)) {
				while(peek().has_value() && peek().value().type != TokenType_t::close_bracket) {
					GString attr_name = try_consume_err(TokenType_t::ident).value.value();
					std::optional<ProcAttr> cur_attr = string_to_PA(attr_name);
					if(!cur_attr.has_value()) {
						ParsingError("unkown Procedure Attribute `" + attr_name + "`");
					}
					stmt_proc->attrs.push_back(cur_attr.value());
					if(peek().has_value() && peek().value().type != TokenType_t::close_bracket) {
						try_consume_err(TokenType_t::comma);
					}
				}
				try_consume_err(TokenType_t::close_bracket);
			}
			stmt_proc->decorators = {};
			while(auto _at_s = try_consume(TokenType_t::at_sign)) {
				Token decorator_name_token = try_consume_err(TokenType_t::ident);
				try_consume_err(TokenType_t::open_paren);
				Token string_literal_token = try_consume_err(TokenType_t::string_lit);
				try_consume_err(TokenType_t::close_paren);
				stmt_proc->decorators[decorator_name_token.value.value()] = string_literal_token.value.value();
			}
			if(peek().has_value() && peek().value().type == TokenType_t::semi) {
				stmt_proc->scope = NULL;
				stmt_proc->prototype = true;
				auto stmt = m_allocator->emplace<NodeStmt>(stmt_proc);
				consume();
				return stmt;
			}
			if(peek().has_value() && peek().value().type == TokenType_t::eq) {
				consume();
				NodeScope* _scope = m_allocator->emplace<NodeScope>();
				if(const auto _stmt = parse_stmt()) {
					_scope->stmts.push_back(_stmt.value());
					stmt_proc->scope = _scope;
				} else {
					error_expected("statement");
				}
			} else {
				if(const auto scope = parse_scope()) {
					stmt_proc->scope = scope.value();
				}
				else {
					ParsingError("after proc name except `{` and `}` after body", -1);
				}
			}
			auto stmt = m_allocator->emplace<NodeStmt>(stmt_proc);
			return stmt;
		}
		if((peek().has_value() && peek().value().type == TokenType_t::ident &&
			peek(1).has_value() && peek(1).value().type == TokenType_t::open_paren)
			|| (
			peek().has_value() && peek().value().type == TokenType_t::ident &&
			peek(1).has_value() && peek(1).value().type == TokenType_t::less
			)) {
			Token identif = try_consume_err(TokenType_t::ident);
			std::optional<Macro> __macro = macro_lookup(identif.value.value());
			if(__macro.has_value()) {
				if(__macro.value().is_pure) ParsingError("can't expand pure macro `" + __macro.value().name + "`");
				GVector<GVector<Token>*>* __args = parse_macro_args();
				Macro _macro = __macro.value();
				expand_macro(_macro, __args, identif);
				return parse_stmt();
			}
			auto stmt_call = m_allocator->emplace<NodeStmtCall>();
			stmt_call->def = identif;
			stmt_call->name = identif.value.value();
			if(peek().has_value() && peek().value().type == TokenType_t::less) {
				consume();
				while(peek().has_value() && peek().value().type != TokenType_t::above) {
					DataType dt = parse_type();
					stmt_call->targs.push_back(dt);
					if(peek().has_value() && peek().value().type != TokenType_t::above) {
						try_consume_err(TokenType_t::comma);
					}
				}
				consume();
			}
			try_consume_err(TokenType_t::open_paren);
			if(peek().has_value() && peek().value().type == TokenType_t::close_paren) {
				stmt_call->args = std::nullopt;
			} else {
				std::pair<NodeExpr*, size_t> pargs = parse_args();
				stmt_call->args = pargs.first;
			}
			try_consume_err(TokenType_t::close_paren);
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>(stmt_call);
			return stmt;
		}
		if(auto ret = try_consume(TokenType_t::_return)) {
			Token def = ret.value();
			auto stmt_return = m_allocator->emplace<NodeStmtReturn>();
			if(peek().has_value() && peek().value().type == TokenType_t::semi) {
				stmt_return->expr = std::nullopt;
			} else {
				if (const auto node_expr = parse_expr()) {
					stmt_return->expr = node_expr.value();
				}
				else {
					error_expected("expression");
				}
			}
			stmt_return->def = def;
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_return;
			return stmt;
		}
		if(auto store8 = try_consume(TokenType_t::store8)) {
			Token def = store8.value();
			auto stmt_st8 = m_allocator->emplace<NodeStmtStore>();
			stmt_st8->size = 8U;
			try_consume_err(TokenType_t::open_paren);
			std::pair<NodeExpr*, size_t> pargs = parse_args();
			if(pargs.second != 2U) {
				ParsingError("ptr, value");
			}
			NodeBinExprArgs* args = std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(pargs.first->var)->var);
			stmt_st8->ptr = args->args[0];
			stmt_st8->expr = args->args[1];
			stmt_st8->def = def;
			try_consume_err(TokenType_t::close_paren);
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_st8;
			return stmt;
		}
		if(auto store16 = try_consume(TokenType_t::store16)) {
			Token def = store16.value();
			auto stmt_st16 = m_allocator->emplace<NodeStmtStore>();
			stmt_st16->size = 16U;
			try_consume_err(TokenType_t::open_paren);
			std::pair<NodeExpr*, size_t> pargs = parse_args();
			if(pargs.second != 2U) {
				ParsingError("ptr, value");
			}
			NodeBinExprArgs* args = std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(pargs.first->var)->var);
			stmt_st16->ptr = args->args[0];
			stmt_st16->expr = args->args[1];
			stmt_st16->def = def;
			try_consume_err(TokenType_t::close_paren);
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_st16;
			return stmt;
		}
		if(auto store32 = try_consume(TokenType_t::store32)) {
			Token def = store32.value();
			auto stmt_st32 = m_allocator->emplace<NodeStmtStore>();
			stmt_st32->size = 32U;
			try_consume_err(TokenType_t::open_paren);
			std::pair<NodeExpr*, size_t> pargs = parse_args();
			if(pargs.second != 2U) {
				ParsingError("ptr, value");
			}
			NodeBinExprArgs* args = std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(pargs.first->var)->var);
			stmt_st32->ptr = args->args[0];
			stmt_st32->expr = args->args[1];
			stmt_st32->def = def;
			try_consume_err(TokenType_t::close_paren);
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_st32;
			return stmt;
		}

		if (auto inc = try_consume(TokenType_t::_include)) {
		    Token str_tok = try_consume_err(TokenType_t::string_lit);
		    GString raw_fname = str_tok.value.value();
		    GString fname = raw_fname + ".bpm";

		    std::filesystem::path current_source_file = inc->file.c_str();
		    std::filesystem::path current_dir = current_source_file.parent_path();
		    GString relative_path = GString((current_dir / fname.c_str()).string().c_str());
		    GString path;

		    if (file_exists(relative_path)) path = std::filesystem::canonical(relative_path.c_str()).string();
		    else if (file_exists(fname))    path = std::filesystem::canonical(fname.c_str()).string();
		    else if (file_exists("lib/" + fname))
		        path = std::filesystem::canonical(std::string("lib/" + fname)).string();
		    else if (__slashinpath && file_exists(basepath + "/" + fname))
		        path = std::filesystem::canonical(std::string(basepath + "/" + fname.c_str())).string();
		    else
		        ParsingError("file not found at `include` - `" + fname + "`");

		    m_preprocessor_stmt = true;
		    if (m_includes.find(path) != m_includes.end()) {
		        return {};
		    }

		    GString contents;
		    {
		        GStringStream contents_stream;
		        std::fstream input(path.c_str(), std::ios::in);
		        contents_stream << input.rdbuf();
		        contents = contents_stream.str();
		        input.close();
		    }

		    Tokenizer nlexer(std::move(contents));
		    auto result = nlexer.tokenize(path);
		    m_includes.insert(path);

		    m_diag_man->save_file(std::move(result.tokens->operator[](0).file), std::move(*result.lines));

		    m_tok_cursor.push_stream(std::move(*result.tokens));

		    return {};
		}

		if(auto buffer = try_consume(TokenType_t::buffer)) {
			Token def = buffer.value();
			auto stmt_buf = m_allocator->emplace<NodeStmtBuffer>();
			Token identif = try_consume_err(TokenType_t::ident);
			try_consume_err(TokenType_t::open_paren);
			stmt_buf->def = def;
			stmt_buf->name = identif.value.value();
			if(auto expr = parse_expr()) {
				stmt_buf->size = expr.value();
			} else {
				error_expected("constant expression");
			}
			try_consume_err(TokenType_t::close_paren);
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_buf;
			return stmt;
		}

		if(auto _asm = try_consume(TokenType_t::_asm)) {
			//Token def = _asm.value();
			auto stmt_asm = m_allocator->emplace<NodeStmtAsm>();
			if(auto _expr = parse_expr()) {
				NodeExpr* expr = _expr.value();
				if(!std::holds_alternative<NodeTerm*>(expr->var)) {
					error_expected("string with asm code");
				}
				NodeTerm* as_term = std::get<NodeTerm*>(expr->var);
				if(!std::holds_alternative<NodeTermStrLit*>(as_term->var)) {
					error_expected("string with asm code");
				}
				NodeTermStrLit* as_str = std::get<NodeTermStrLit*>(as_term->var);
				stmt_asm->code = as_str->str_lit;
			} else {
				error_expected("string with asm code");
			}
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_asm;
			return stmt;
		}

		if(auto _cextern = try_consume(TokenType_t::cextern)) {
			//Token def = _cextern.value();
			auto stmt_cextern = m_allocator->emplace<NodeStmtCextern>();
			stmt_cextern->name = try_consume_err(TokenType_t::string_lit).value.value();
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_cextern;
			return stmt;
		}

		if(auto _push = try_consume(TokenType_t::pushonstack)) {
			Token def = _push.value();
			auto stmt_push = m_allocator->emplace<NodeStmtPushOnStack>();
			stmt_push->def = def;
			try_consume_err(TokenType_t::open_paren);
			if(auto _expr = parse_expr()) {
				stmt_push->expr = _expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType_t::close_paren);
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_push;
			return stmt;
		}

		if(auto _empty = try_consume(TokenType_t::empty_stmt)) {
			m_preprocessor_stmt = true;
			if (expect_semi) try_consume_err(TokenType_t::semi);
			return {};
		}

		if(auto _cns = try_consume(TokenType_t::_const)) {
			auto stmt_const = m_allocator->emplace<NodeStmtConst>();
			stmt_const->def = _cns.value();
			GString name = try_consume_err(TokenType_t::ident).value.value();
			stmt_const->name = name;
			try_consume_err(TokenType_t::eq);
			if(const auto _expr = parse_expr()) {
				stmt_const->expr = _expr.value();
			} else {
				error_expected("expression");
			}
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_const;
			return stmt;
		}

		if(auto _tdef = try_consume(TokenType_t::_typedef)) {
			auto stmt_tdef = m_allocator->emplace<NodeStmtTypedef>();
			stmt_tdef->def = _tdef.value();
			GString name = try_consume_err(TokenType_t::ident).value.value();
			stmt_tdef->name = name;
			try_consume_err(TokenType_t::eq);
			stmt_tdef->type = parse_type();
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_tdef;
			return stmt;
		}

		if(auto _struct = try_consume(TokenType_t::_struct)) {
			Token def = _struct.value();
			auto stmt_struct = m_allocator->emplace<NodeStmtStruct>();
			Token nametok = try_consume_err(TokenType_t::ident);
			stmt_struct->name = nametok.value.value();
			stmt_struct->def = def;
			stmt_struct->__allocator = std::nullopt;
			stmt_struct->temp = false;
			if(peek().has_value() && peek().value().type == TokenType_t::open_paren) {
				consume();
				Token allc_id = try_consume_err(TokenType_t::ident);
				try_consume_err(TokenType_t::close_paren);
				stmt_struct->__allocator = allc_id.value.value();
			}

			if(peek().has_value() && peek().value().type == TokenType_t::less) {
				consume();
				while(peek().has_value() && peek().value().type != TokenType_t::above) {
					stmt_struct->temps.push_back(try_consume_err(TokenType_t::ident).value.value());
					if(peek().has_value() && peek().value().type != TokenType_t::above) {
						try_consume_err(TokenType_t::comma);
					}
				}
				consume();
				stmt_struct->temp = true;
			}

			if (peek().has_value() && peek().value().type == TokenType_t::double_dot) {
            	consume();
            	stmt_struct->parent = parse_type();
        	}

			try_consume_err(TokenType_t::open_curly);
			while(peek().has_value() && peek().value().type != TokenType_t::close_curly) {
				Token ident = try_consume_err(TokenType_t::ident);
				try_consume_err(TokenType_t::double_dot);
				DataType dtype(parse_type());
				if (dtype.is_object() && dtype.getobjectname() == stmt_struct->name) {
                    if (dtype.root().ptrlvl == 0 && !dtype.root().link) {
                    	m_diag_man->DiagnosticMessage(ident, "error", "Recursive struct definition `" + stmt_struct->name + 
                                       "` contains itself without pointer or reference indirection.", 0);
                    	m_diag_man->DiagnosticMessage(nametok, "note", "Structure defined here.", 0);
                    	exit(EXIT_FAILURE);
                    }
                }
				stmt_struct->fields.push_back(std::make_pair(ident.value.value(), dtype));
				if(peek().has_value() && peek().value().type != TokenType_t::close_curly) {
					try_consume_err(TokenType_t::comma);
				}
			}
			try_consume_err(TokenType_t::close_curly);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_struct;
			return stmt;
		}

		if (auto _interface = try_consume(TokenType_t::_interface)) {
		    Token def = _interface.value();
		    auto stmt_interface = m_allocator->emplace<NodeStmtInterface>();
		    stmt_interface->name = try_consume_err(TokenType_t::ident).value.value();
		    stmt_interface->def  = def;

		    if (peek().has_value() && peek().value().type == TokenType_t::less) {
		        consume();
		        while (peek().has_value() && peek().value().type != TokenType_t::above) {
		            stmt_interface->temps.push_back(try_consume_err(TokenType_t::ident).value.value());
		            if (peek().has_value() && peek().value().type != TokenType_t::above) {
		                try_consume_err(TokenType_t::comma);
		            }
		        }
		        consume();
		    }

		    try_consume_err(TokenType_t::open_curly);
		    while (peek().has_value() && peek().value().type != TokenType_t::close_curly) {

		        try_consume_err(TokenType_t::proc);
		        Token mnameTok = try_consume_err(TokenType_t::ident);

		        InterfaceMethod m;
		        m.def  = mnameTok;
		        m.name = mnameTok.value.value();

				try_consume_err(TokenType_t::open_paren);
				GVector<std::pair<GString, DataType>> params;
				if (peek().has_value() && peek().value().type != TokenType_t::close_paren) {
				    while (true) {
				        if (peek().has_value() &&
				            peek().value().type == TokenType_t::ident &&
				            peek().value().value.has_value() &&
				            peek().value().value.value() == "self" &&
				            peek(1).has_value() &&
				            (peek(1).value().type == TokenType_t::comma ||
				             peek(1).value().type == TokenType_t::close_paren))
				        {
				            Token selfTok = consume();
				            BaseDataType bt;
				            bt = GString("self");
				            DataType dt(bt);
				            params.emplace_back(selfTok.value.value(), dt);
				        } else {
				            DataType ptype = parse_type();
				            Token pid      = try_consume_err(TokenType_t::ident);
				            params.emplace_back(pid.value.value(), ptype);
				        }

				        if (!peek().has_value() || peek().value().type == TokenType_t::close_paren)
				            break;
				        try_consume_err(TokenType_t::comma);
				    }
				}
				try_consume_err(TokenType_t::close_paren);

		        DataType rettype = BaseDataTypeVoid;
		        if (peek().has_value() && peek().value().type == TokenType_t::arrow) {
		            consume();
		            rettype = parse_type();
		        }

		        m.params  = std::move(params);
		        m.rettype = rettype;

		        try_consume_err(TokenType_t::semi);

		        stmt_interface->methods.push_back(m);
		    }
		    try_consume_err(TokenType_t::close_curly);

		    auto stmt = m_allocator->emplace<NodeStmt>();
		    stmt->var = stmt_interface;
		    return stmt;
		}

		if(auto _st_assert = try_consume(TokenType_t::_static_assert)) {
			try_consume_err(TokenType_t::open_paren);
			auto stmt_st = m_allocator->emplace<NodeStmtStaticAssert>();
			stmt_st->def = _st_assert.value();
			if(auto _expr = parse_expr()) {
				stmt_st->condition = _expr.value();
			} else {
				error_expected("expression");
			}

			try_consume_err(TokenType_t::comma);
			
			GString _err_str = try_consume_err(TokenType_t::string_lit).value.value();
			
			stmt_st->msg = _err_str;

			try_consume_err(TokenType_t::close_paren);

			if (expect_semi) try_consume_err(TokenType_t::semi);
			
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_st;
			return stmt;
		}

		if(auto _try = try_consume(TokenType_t::_try)) {
			auto stmt_try = m_allocator->emplace<NodeStmtTry>();
			stmt_try->def = _try.value();
			if(const auto _scope = parse_scope()) {
				stmt_try->_try = _scope.value();
			} else error_expected("try scope");
			try_consume_err(TokenType_t::_catch);
			try_consume_err(TokenType_t::open_paren);
			stmt_try->name = try_consume_err(TokenType_t::ident).value.value();
			try_consume_err(TokenType_t::double_dot);
			stmt_try->type = parse_type();
			try_consume_err(TokenType_t::close_paren);
			if(const auto _scope = parse_scope()) {
				stmt_try->_catch = _scope.value();
			} else error_expected("catch scope");
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_try;
			return stmt;
		}

		if(auto del = try_consume(TokenType_t::_delete)) {
			Token def = del.value();
			auto stmt_delete = m_allocator->emplace<NodeStmtDelete>();
			if (const auto node_expr = parse_expr()) {
				stmt_delete->expr = node_expr.value();
			}
			else {
				error_expected("expression");
			}
			stmt_delete->def = def;
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_delete;
			return stmt;
		}

		if(auto _raise = try_consume(TokenType_t::raise)) {
			Token def = _raise.value();
			auto stmt_raise = m_allocator->emplace<NodeStmtRaise>();
			if (const auto node_expr = parse_expr()) {
				stmt_raise->expr = node_expr.value();
			}
			else {
				error_expected("expression");
			}
			stmt_raise->def = def;
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_raise;
			return stmt;
		}

		if(auto _break = try_consume(TokenType_t::_break)) {
			Token def = _break.value();
			auto stmt_break = m_allocator->emplace<NodeStmtBreak>();
			stmt_break->def = def;
			if (expect_semi) try_consume_err(TokenType_t::semi);
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_break;
			return stmt;
		}

		if(auto _hash = try_consume(TokenType_t::hash_sign)) {
			Token _prep = consume();
			if(_prep.type == TokenType_t::_define) {
				m_preprocessor_stmt = true;
				GString mname = try_consume_err(TokenType_t::ident).value.value();
				bool pure = false;
				GVector<GString> __args;
				if(peek().has_value() && peek().value().type == TokenType_t::semi) {
					pure = true;
				}
				else {
					if(peek().has_value() && peek().value().type == TokenType_t::open_paren) {
						consume();
						while(peek().has_value() && peek().value().type != TokenType_t::close_paren) {
							GString name = try_consume_err(TokenType_t::ident).value.value();
							__args.push_back(name);
						}
						consume();
					}
				}
				Macro __macro = { .name = mname, .args = __args, .body = {}, .is_pure = pure };
				if(!pure) {
					while(peek().has_value() && peek().value().type != TokenType_t::dollar) {
						Token ctok = consume();
						ctok.expand = m_allocator->emplace<Token>(ctok);
						__macro.body.push_back(ctok);
					}
				}
				consume();
				m_macroses[mname] = __macro;
				return {};
			}
			else if(_prep.type == TokenType_t::if_) {
				auto stmt_ctif = m_allocator->emplace<NodeStmtCompileTimeIf>();
				try_consume_err(TokenType_t::open_paren);
				if(auto _expr = parse_expr()) {
					stmt_ctif->condition = _expr.value();
				} else {
					error_expected("expression");
				}
				try_consume_err(TokenType_t::close_paren);
				stmt_ctif->def = _prep;
				if(const auto _scope = parse_scope()) {
					stmt_ctif->_if = _scope.value();
				}
				if(auto _else = try_consume(TokenType_t::else_)) {
					if(const auto _scope = parse_scope()) {
						stmt_ctif->_else = _scope.value();
					}
				}
				auto stmt = m_allocator->emplace<NodeStmt>();
				stmt->var = stmt_ctif;
				return stmt;
			}
			else {
				error_expected("preprocessor command");
			}
		}

        if (peek().has_value() &&
            peek().value().type == TokenType_t::ident &&
            peek(1).has_value() &&
            peek(1).value().type == TokenType_t::double_colon)
        {
            Token first = try_consume_err(TokenType_t::ident);
            GVector<GString> segments;
            segments.push_back(first.value.value());

            while (peek().has_value() && peek().value().type == TokenType_t::double_colon) {
                consume();
                Token segTok = try_consume_err(TokenType_t::ident);
                segments.push_back(segTok.value.value());
            }

            if (!peek().has_value() ||
                (peek().value().type != TokenType_t::less &&
                 peek().value().type != TokenType_t::open_paren))
            {
                ParsingError("excepted `(` or `<` after qualified name");
            }

            if (segments.size() < 2) {
                ParsingError("invalid qualified name");
            }

            GString funcName = segments.back();
            segments.pop_back();

            GString nsName;
            for (size_t i = 0; i < segments.size(); ++i) {
                if (i) nsName += "::";
                nsName += segments[i];
            }

            auto stmt_call = m_allocator->emplace<NodeStmtNmCall>();
            stmt_call->def  = first;
            stmt_call->nm   = segments;   
            stmt_call->name = funcName; 

            if (peek().has_value() && peek().value().type == TokenType_t::less) {
                consume();
                while (peek().has_value() && peek().value().type != TokenType_t::above) {
                    DataType dt = parse_type();
                    stmt_call->targs.push_back(dt);
                    if (peek().has_value() && peek().value().type != TokenType_t::above) {
                        try_consume_err(TokenType_t::comma);
                    }
                }
                try_consume_err(TokenType_t::above);
            }

            try_consume_err(TokenType_t::open_paren);
            if (peek().has_value() && peek().value().type == TokenType_t::close_paren) {
                stmt_call->args = std::nullopt;
            } else {
                std::pair<NodeExpr*, size_t> pargs = parse_args();
                stmt_call->args = pargs.first;
            }
            try_consume_err(TokenType_t::close_paren);
            if (expect_semi) try_consume_err(TokenType_t::semi);

            auto stmt = m_allocator->emplace<NodeStmt>(stmt_call);
            return stmt;
        }

		if(auto _namespace = try_consume(TokenType_t::_namespace)) {
			auto stmt_space = m_allocator->emplace<NodeStmtNamespace>();
			stmt_space->def = _namespace.value();
			stmt_space->name = try_consume_err(TokenType_t::ident).value.value();
			if(auto _scope = parse_scope()) {
				stmt_space->scope = _scope.value();
			} else {
				error_expected("scope");
			}
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_space;
			return stmt;
		}

		if(auto impl = try_consume(TokenType_t::impl)) {
			auto stmt_impl = m_allocator->emplace<NodeStmtImpl>();
			stmt_impl->def = impl.value();
			stmt_impl->name = try_consume_err(TokenType_t::ident).value.value();
			if(auto open_ls = try_consume(TokenType_t::less)) {
				while(peek().has_value() && peek().value().type != TokenType_t::above) {
					stmt_impl->temps.push_back(try_consume_err(TokenType_t::ident).value.value());
					if(peek().has_value() && peek().value().type != TokenType_t::above) {
						try_consume_err(TokenType_t::comma);
					}
				}
				consume();
			}
			if(auto bracket = try_consume(TokenType_t::open_bracket)) {
				while(peek().has_value() && peek().value().type != TokenType_t::close_bracket) {
					stmt_impl->inst.insert(try_consume_err(TokenType_t::ident).value.value());
					if(peek().has_value() && peek().value().type != TokenType_t::close_bracket) {
						try_consume_err(TokenType_t::comma);
					}
				}
				consume();
			}
			if(auto _scope = parse_scope()) {
				stmt_impl->scope = _scope.value();
			} else {
				error_expected("scope");
			}
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_impl;
			return stmt;
		}

		if(auto _oninit = try_consume(TokenType_t::oninit)) {
			auto stmt_oninit = m_allocator->emplace<NodeStmtOninit>();
			stmt_oninit->def = _oninit.value();
			if(auto _scope = parse_scope()) {
				stmt_oninit->scope = _scope.value();
			}
			else {
				error_expected("scope");
			}
			auto stmt = m_allocator->emplace<NodeStmt>();
			stmt->var = stmt_oninit;
			return stmt;
		}

		if (auto _for = try_consume(TokenType_t::for_)) {
            try_consume_err(TokenType_t::open_paren);
            
            auto stmt_for = m_allocator->emplace<NodeStmtFor>();
            
            if (peek().value().type != TokenType_t::semi) {
                if (auto init = parse_stmt(true)) {
                    stmt_for->init = init.value();
                } else {
                    error_expected("statement in for-init");
                }
            } else {
                consume();
                stmt_for->init = nullptr;
            }
            
            if (peek().value().type != TokenType_t::semi) {
                if (auto cond = parse_expr()) {
                    stmt_for->cond = cond.value();
                } else {
                    error_expected("expression in for-cond");
                }
            } else {
                stmt_for->cond = nullptr;
            }
            try_consume_err(TokenType_t::semi);
            
            if (peek().value().type != TokenType_t::close_paren) {
                if (auto step = parse_stmt(false)) {
                    stmt_for->step = step.value();
                } else {
                    error_expected("statement in for-step");
                }
            } else {
                stmt_for->step = nullptr;
            }
            
            try_consume_err(TokenType_t::close_paren);
            
            if (auto scope = parse_scope()) {
                stmt_for->scope = scope.value();
            } else {
                error_expected("scope for loop body");
            }
            
            auto stmt = m_allocator->emplace<NodeStmt>(stmt_for);
            return stmt;
		}

		if (try_consume(TokenType_t::foreach_)) {
            try_consume_err(TokenType_t::open_paren);

            try_consume_err(TokenType_t::let);
            
            Token var_tok = try_consume_err(TokenType_t::ident);
            
            try_consume_err(TokenType_t::double_dot);
            
            NodeExpr* expr = nullptr;
            if (auto e = parse_expr()) {
                expr = e.value();
            } else {
                error_expected("container expression");
            }
            
            try_consume_err(TokenType_t::close_paren);
            
            NodeScope* scope = nullptr;
            if (auto s = parse_scope()) {
                scope = s.value();
            } else {
                error_expected("scope");
            }
            
            auto stmt_foreach = m_allocator->emplace<NodeStmtForeach>();
            stmt_foreach->var_name = var_tok;
            stmt_foreach->expr = expr;
            stmt_foreach->scope = scope;
            
            auto stmt = m_allocator->emplace<NodeStmt>(stmt_foreach);
            return stmt;
        }

        if (auto _enum = try_consume(TokenType_t::enum_)) {
            auto stmt_enum = m_allocator->emplace<NodeStmtEnum>();
            stmt_enum->def = _enum.value();
            stmt_enum->name = try_consume_err(TokenType_t::ident).value.value();
            
            try_consume_err(TokenType_t::open_curly);
            
            int counter = 0;
            while(peek().has_value() && peek().value().type != TokenType_t::close_curly) {
                GString member_name = try_consume_err(TokenType_t::ident).value.value();
                int value = counter;
                
                if (peek().has_value() && peek().value().type == TokenType_t::eq) {
                    consume();
                    Token val_tok = try_consume_err(TokenType_t::int_lit);
                    value = std::stol(val_tok.value.value().c_str());
                    counter = value; 
                }
                
                stmt_enum->members.push_back({member_name, value});
                counter++;
                
                if (peek().has_value() && peek().value().type != TokenType_t::close_curly) {
                    try_consume_err(TokenType_t::comma);
                }
            }
            
            try_consume_err(TokenType_t::close_curly);
			auto stmt = m_allocator->emplace<NodeStmt>(stmt_enum);
			return stmt;
		}

		if (auto lvalue = parse_expr(0, true)) {
            if (!peek().has_value()) {
                error_expected("statement");
            }
            Token curtok = peek().value();
            if (!lvalue.has_value()) {
                error_expected("lvalue");
            }

            if (curtok.type == TokenType_t::eq) {
                const auto stmt_assign = m_allocator->emplace<NodeStmtAssign>();
                stmt_assign->lvalue = lvalue.value();
                stmt_assign->def = consume();
                if (const auto expr = parse_expr()) {
                    stmt_assign->expr = expr.value();
                } else {
                    error_expected("expression");
                }
                if (expect_semi) try_consume_err(TokenType_t::semi);
                auto stmt = m_allocator->emplace<NodeStmt>(stmt_assign);
                return stmt;
            }
            else if (curtok.type == TokenType_t::plus_eq) {
                const auto stmt_assign = m_allocator->emplace<NodeStmtIncBy>();
                stmt_assign->lvalue = lvalue.value();
                stmt_assign->def = consume();
                if (const auto expr = parse_expr()) {
                    stmt_assign->expr = expr.value();
                } else {
                    error_expected("expression");
                }
                if (expect_semi) try_consume_err(TokenType_t::semi);
                auto stmt = m_allocator->emplace<NodeStmt>(stmt_assign);
                return stmt;
            }
            else if (curtok.type == TokenType_t::minus_eq) {
                const auto stmt_assign = m_allocator->emplace<NodeStmtDecBy>();
                stmt_assign->lvalue = lvalue.value();
                stmt_assign->def = consume();
                if (const auto expr = parse_expr()) {
                    stmt_assign->expr = expr.value();
                } else {
                    error_expected("expression");
                }
                if (expect_semi) try_consume_err(TokenType_t::semi);
                auto stmt = m_allocator->emplace<NodeStmt>(stmt_assign);
                return stmt;
            }
            else if (curtok.type == TokenType_t::star_eq) {
                const auto stmt_assign = m_allocator->emplace<NodeStmtMulBy>();
                stmt_assign->lvalue = lvalue.value();
                stmt_assign->def = consume();
                if (const auto expr = parse_expr()) {
                    stmt_assign->expr = expr.value();
                } else {
                    error_expected("expression");
                }
                if (expect_semi) try_consume_err(TokenType_t::semi);
                auto stmt = m_allocator->emplace<NodeStmt>(stmt_assign);
                return stmt;
            }
            else if (curtok.type == TokenType_t::fslash_eq) {
                const auto stmt_assign = m_allocator->emplace<NodeStmtDivBy>();
                stmt_assign->lvalue = lvalue.value();
                stmt_assign->def = consume();
                if (const auto expr = parse_expr()) {
                    stmt_assign->expr = expr.value();
                } else {
                    error_expected("expression");
                }
                if (expect_semi) try_consume_err(TokenType_t::semi);
                auto stmt = m_allocator->emplace<NodeStmt>(stmt_assign);
                return stmt;
            }
            else if (curtok.type == TokenType_t::semi) {
                NodeExpr* expr = lvalue.value();

                if (std::holds_alternative<NodeBinExpr*>(expr->var)) {
                    NodeBinExpr* bexpr = std::get<NodeBinExpr*>(expr->var);
                    if (std::holds_alternative<NodeBinExprDot*>(bexpr->var)) {
                        NodeBinExprDot* dot = std::get<NodeBinExprDot*>(bexpr->var);

                        if (std::holds_alternative<NodeTerm*>(dot->rhs->var)) {
                            NodeTerm* rhs_term = std::get<NodeTerm*>(dot->rhs->var);
                            if (std::holds_alternative<NodeTermCall*>(rhs_term->var)) {
                                NodeTermCall* call = std::get<NodeTermCall*>(rhs_term->var);

                                auto stmt_call = m_allocator->emplace<NodeStmtMtCall>();
                                stmt_call->def   = call->def;
                                stmt_call->mt    = dot->lhs;
                                stmt_call->name  = call->name;
                                stmt_call->targs = call->targs;

                                auto* arglist = m_allocator->emplace<NodeBinExprArgs>();
                                arglist->args.push_back(dot->lhs);

                                if (call->args.has_value()) {
                                    NodeExpr* aexpr = call->args.value();
                                    if (std::holds_alternative<NodeBinExpr*>(aexpr->var) &&
                                        std::holds_alternative<NodeBinExprArgs*>(
                                            std::get<NodeBinExpr*>(aexpr->var)->var
                                        ))
                                    {
                                        NodeBinExpr* argsBin = std::get<NodeBinExpr*>(aexpr->var);
                                        NodeBinExprArgs* argsNode =
                                            std::get<NodeBinExprArgs*>(argsBin->var);
                                        for (NodeExpr* e : argsNode->args) {
                                            arglist->args.push_back(e);
                                        }
                                    } else {
                                        arglist->args.push_back(aexpr);
                                    }
                                }

                                NodeBinExpr* ab = m_allocator->emplace<NodeBinExpr>();
                                ab->def = call->def;
                                ab->var = arglist;
                                NodeExpr* argsExpr = m_allocator->emplace<NodeExpr>();
                                argsExpr->var = ab;
                                stmt_call->args = argsExpr;

                                if (expect_semi) try_consume_err(TokenType_t::semi);
                                auto stmt = m_allocator->emplace<NodeStmt>(stmt_call);
                                return stmt;
                            }
                        }
                    }
                }

                error_expected("statement");
            }
            else {
                error_expected("statement");
            }
        }

        return {};
	}

	std::optional<NodeProg*> parse_prog()
	{
		hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
		NodeProg* prog = new NodeProg;
		while (peek().has_value()) {
			if (auto stmt = parse_stmt()) {
				prog->stmts.push_back(stmt.value());
			}
			else {
				if(m_preprocessor_stmt) {
					m_preprocessor_stmt = false;
				} else {
					error_expected("statement");
				}
			}
		}
		return prog;
	}

private:
	TokenStreamCursor m_tok_cursor;

    [[ nodiscard ]] std::optional<Token> peek(const int offset = 0) const {
        return m_tok_cursor.peek(offset);
    }

    Token consume() {
        return m_tok_cursor.consume();
    }

    std::optional<Token> try_consume(const TokenType_t type) {
        auto t = m_tok_cursor.peek();
        if (t.has_value() && t->type == type) {
            return m_tok_cursor.consume();
        }
        return std::nullopt;
    }

    Token try_consume_err(const TokenType_t type) {
        auto t = m_tok_cursor.peek();
        if (t.has_value() && t->type == type) {
            return m_tok_cursor.consume();
        }
        error_expected(tok_to_string(type));
        Token fake;
        return fake;
    }
    ArenaAllocator* m_allocator = nullptr;
    DiagnosticManager* m_diag_man;
	GSet<GString> m_includes;
	GMap<GString, Macro> m_macroses;
	bool m_preprocessor_stmt = false;
	size_t CTX_IOTA = 0ULL;
};