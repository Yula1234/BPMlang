#pragma once

#include "tokenization.hpp"
#include "arena.hpp"

enum class SimpleDataType {
	_int,
	ptr,
	_void,
	any,
};

struct DataType {
	bool is_object = false;
	std::variant<SimpleDataType, std::string> type;
	bool is_simple() const {
		return !is_object;
	}
	SimpleDataType getsimpletype() const {
		return std::get<SimpleDataType>(this->type);
	}
	std::string getobjectname() const {
		return std::get<std::string>(this->type);
	}
	std::string to_string() const {
		if(!this->is_object) {
			switch(this->getsimpletype()) {
			case SimpleDataType::_int:
				return "`int`";
			case SimpleDataType::ptr:
				return "`ptr`";
			case SimpleDataType::_void:
				return "`void`";
			case SimpleDataType::any:
				return "`any`";
			default:
				break;
			}
			assert(false);
		} else {
			return "`object<" + this->getobjectname() + ">`";
		}
	}
	bool eq(const DataType& two) const {
		if(two.is_simple() && two.getsimpletype() == SimpleDataType::any) {
			return true;
		}
		if((this->is_object && !two.is_object) || (!this->is_object && two.is_object)) {
			return false;
		}
		if(this->is_object && two.is_object) {
			return this->getobjectname() == two.getobjectname();
		}
		else if(!this->is_object && !two.is_object) {
			return this->getsimpletype() == two.getsimpletype();
		} else {
			assert(false); // maybe bug
		}
	}
	bool operator==(const DataType& two) const {
		return this->eq(two);
	}
	bool operator!=(const DataType& two) const {
		return !this->eq(two);
	}
	DataType() {}
	DataType(SimpleDataType other) {
		this->type = other;
		this->is_object = false;
	}
	DataType(const DataType& other) {
		this->type = other.type;
		this->is_object = other.is_object;
	}
	DataType(std::string objname) {
		this->type = objname;
		this->is_object = true;
	}
	void operator=(SimpleDataType other) {
		type = other;
		is_object = false;
	}
	void operator=(const DataType& other) {
		type = other.type;
		is_object = other.is_object;
	}
	void operator=(std::string objname) {
		type = objname;
		is_object = true;
	}
};

DataType make_int_type() {
	DataType tp = SimpleDataType::_int;
	return tp;
}

DataType make_ptr_type() {
	DataType tp = SimpleDataType::ptr;
	return tp;
}

DataType make_void_type() {
	DataType tp = SimpleDataType::_void;
	return tp;
}

DataType make_any_type() {
	DataType tp = SimpleDataType::any;
	return tp;
}

DataType DataTypeInt = make_int_type();
DataType DataTypePtr = make_ptr_type();
DataType DataTypeVoid = make_void_type();
DataType DataTypeAny = make_any_type();

#define yforeach(container) for(int i = 0;i < static_cast<int>(container.size());++i)

std::string dt_to_string(DataType dt) {
	return dt.to_string();
}

DataType token_to_dt(TokenType tt) {
	switch(tt) {
	case TokenType::int_type:
		return DataTypeInt;
	case TokenType::ptr_type:
		return DataTypePtr;
	case TokenType::void_type:
		return DataTypeVoid;
	case TokenType::any_type:
		return DataTypeAny;
	default:
		break;
	}
	assert(false); // unreacheable
}

DataType uni_token_to_dt(Token tok) {
	if(tok.type == TokenType::ident) {
		DataType dt;
		dt = tok.value.value();
		return dt;
	}
	return token_to_dt(tok.type);
}

bool is_type_token(TokenType tp) {
	return (tp == TokenType::int_type || tp == TokenType::ptr_type || tp == TokenType::void_type || tp == TokenType::any_type || tp == TokenType::ident);
}

std::ostream& operator<<(std::ostream& out, const DataType dt) {
	std::cout << dt_to_string(dt); 
	return out;
}

enum class ProcAttr {
	nostdargs,
	noprolog,
	nosizedargs
};

std::optional<ProcAttr> string_to_PA(std::string str) {
	if(str == "nostdargs") {
		return ProcAttr::nostdargs;
	}
	if(str == "noprolog") {
		return ProcAttr::noprolog;
	}
	if(str == "nosizedargs") {
		return ProcAttr::nosizedargs;
	}
	return std::nullopt;
}

struct NodeTermIntLit {
	Token int_lit;
};

struct NodeTermStrLit {
	std::string str_lit;
};

struct NodeTermIdent {
	Token ident;
};

struct NodeExpr;

struct NodeTermAmpersand {
	Token def;
	NodeExpr* expr;
};

struct NodeTermParen {
	NodeExpr* expr;
};

struct NodeTermCall {
	Token def; 
	std::string name;
	std::optional<NodeExpr*> args;
};

struct NodeTermSizeof {
	Token def;
	DataType type;
};

struct NodeTermTypeid {
	Token def;
	std::optional<DataType> ptype;
	NodeExpr* expr;
};

struct NodeTermRd {
	Token def;
	size_t size;
	NodeExpr* expr;
};

struct NodeTermCast {
	Token def;
	DataType type;
	NodeExpr* expr;
};

struct NodeBinExprAdd {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprMulti {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprSub {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprDiv {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprLess {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprAbove {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprEqEq {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprNotEq {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprMod {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprAnd {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprOr {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprShl {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprShr {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprArgs {
	std::vector<NodeExpr*> args;
};

struct NodeBinExprDot {
	NodeExpr* lhs; // object
	NodeExpr* rhs; // field (must be a NodeTermIdent*)
};

struct NodeBinExpr {
	Token def;
	std::variant<NodeBinExprAdd*, NodeBinExprMulti*, NodeBinExprSub*, NodeBinExprDiv*, NodeBinExprEqEq*, NodeBinExprLess*, NodeBinExprAbove*, NodeBinExprArgs*, NodeBinExprNotEq*, NodeBinExprMod*, NodeBinExprDot*, NodeBinExprAnd*, NodeBinExprOr*, NodeBinExprShl*, NodeBinExprShr*> var;
};

struct NodeTerm {
	std::variant<NodeTermIntLit*, NodeTermStrLit*, NodeTermIdent*, NodeTermParen*, NodeTermCall*, NodeTermRd*, NodeTermAmpersand*, NodeTermCast*, NodeTermSizeof*, NodeTermTypeid*> var;
};

struct NodeExpr {
	std::variant<NodeTerm*, NodeBinExpr*> var;
};

struct NodeStmtExit {
	Token def;
	NodeExpr* expr;
};

struct NodeStmtReturn {
	Token def;
	std::optional<NodeExpr*> expr;
};

struct NodeStmtLetNoAssign {
	Token ident;
	DataType type;
};

struct NodeStmtLet {
	Token ident;
	NodeExpr* expr {};
};

struct NodeStmt;

struct NodeScope {
	std::vector<NodeStmt*> stmts;
};

struct NodeIfPred;

struct NodeIfPredElif {
	NodeExpr* expr {};
	NodeScope* scope {};
	std::optional<NodeIfPred*> pred;
};

struct NodeIfPredElse {
	NodeScope* scope;
};

struct NodeIfPred {
	std::variant<NodeIfPredElif*, NodeIfPredElse*> var;
};

struct NodeStmtIf {
	NodeExpr* expr {};
	NodeScope* scope {};
	std::optional<NodeIfPred*> pred;
};

struct NodeStmtAssign {
	Token def;
	NodeExpr* lvalue {};
	NodeExpr* expr {};
};

struct NodeStmtWhile {
	NodeScope* scope {};
	NodeExpr* expr {};
};

struct NodeStmtProc {
	std::string name;
	Token def;
	DataType rettype;
	std::vector<ProcAttr> attrs;
	std::vector<std::pair<std::string, DataType>> params;
	NodeScope* scope {};
	bool prototype = false;
};

struct NodeStmtCall {
	Token def;
	std::string name;
	std::optional<NodeExpr*> args;
};

struct NodeStmtStore {
	Token def;
	NodeExpr* expr;
	NodeExpr* ptr;
	size_t size;
};

struct NodeStmtBuffer {
	Token def;
	std::string name;
	size_t size;
};

struct NodeStmtCextern {
	std::string name;
};

struct NodeStmtAsm {
	std::string code;
};

struct NodeStmtStruct {
	Token def;
	std::string name;
	std::vector<std::pair<std::string, DataType>> fields;
	std::optional<std::string> __allocator;
};

struct NodeStmtInterface {
	Token def;
	std::string name;
	std::vector<std::pair<std::string, DataType>> fields;
};

struct NodeStmtDelete {
	Token def;
	NodeExpr* expr;
};

struct NodeStmtBreak {
	Token def;
};

struct NodeStmtIncBy {
	Token def;
	NodeExpr* expr;
	NodeExpr* lvalue;
};

struct NodeStmtDecBy {
	Token def;
	NodeExpr* expr;
	NodeExpr* lvalue;
};

struct NodeStmtMulBy {
	Token def;
	NodeExpr* expr;
	NodeExpr* lvalue;
};

struct NodeStmtDivBy {
	Token def;
	NodeExpr* expr;
	NodeExpr* lvalue;
};

struct NodeStmtOninit {
	Token def;
	NodeScope* scope;
};

struct NodeStmtPushOnStack {
	Token def;
	NodeExpr* expr;
};

struct NodeStmt {
	std::variant<NodeStmtExit*, NodeStmtLet*,
				NodeScope*, NodeStmtIf*,
				NodeStmtAssign*,NodeStmtAsm*,
				NodeStmtProc*, NodeStmtCall*,
				NodeStmtWhile*,NodeStmtReturn*,
				NodeStmtStore*,NodeStmtBuffer*,
				NodeStmtCextern*,NodeStmtStruct*,
				NodeStmtDelete*,NodeStmtLetNoAssign*,
				NodeStmtBreak*,NodeStmtIncBy*,
				NodeStmtDecBy*,NodeStmtMulBy*,
				NodeStmtDivBy*,NodeStmtInterface*,
				NodeStmtOninit*,NodeStmtPushOnStack*> var;
};

struct NodeProg {
	std::vector<NodeStmt*> stmts {};
};

bool file_exists(std::string name) {
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
	}
	namespace is {
		bool ident(NodeExpr* expr) {
			if(!std::holds_alternative<NodeTerm*>(expr->var)) {
				return false;
			}
			return std::holds_alternative<NodeTermIdent*>(std::get<NodeTerm*>(expr->var)->var);
		}
	}
	namespace get {
		std::optional<NodeTermIdent*> ident(NodeExpr* expr) {
			if(!is::ident(expr)) return std::nullopt;
			return as::ident(expr);
		}
	}
}

struct Constant {
	std::string name;
	int value;
};

struct Macro {
	std::string name;
	std::vector<std::string> args;
	std::vector<Token> body;
};

class Parser {
public:

	explicit Parser(std::vector<Token> tokens)
		: m_tokens(std::move(tokens))
		, m_allocator(1024 * 1024 * 48) // 48 mb
	{
	}

	std::optional<Constant> const_lookup(std::string name) {
		const auto& search = m_consts.find(name);
		if(search != m_consts.end()) {
			return search->second;
		}
		return std::nullopt;
	}

	std::optional<Macro> macro_lookup(std::string name) {
		const auto& search = m_macroses.find(name);
		if(search != m_macroses.end()) {
			return search->second;
		}
		return std::nullopt;
	}

	void ParsingError(const std::string& msg, const int pos = 0) const
	{
		putloc(peek(pos).value());
		std::cout << " ERROR: " << msg << "\n";
		exit(EXIT_FAILURE);
	}

	void error_expected(const std::string& msg) const
	{
		putloc(peek(-1).value());
		if(peek().has_value()) {
			std::cout << " ERROR: excepted " << msg << ", but got " << tok_to_string(peek().value().type) << "\n";
		} else {
			std::cout << " ERROR: excepted " << msg << ", but got nothing\n";
		}
		exit(EXIT_FAILURE);
	}

	int eval_int_value(NodeExpr* expr) {
		int result = 0;
		if(std::holds_alternative<NodeTerm*>(expr->var)) {
			NodeTerm* nterm = std::get<NodeTerm*>(expr->var);
			if(std::holds_alternative<NodeTermIntLit*>(nterm->var)) {
				return std::stoul(std::get<NodeTermIntLit*>(nterm->var)->int_lit.value.value());
			}
			if(std::holds_alternative<NodeTermIdent*>(nterm->var)) {
				std::string cname = std::get<NodeTermIdent*>(nterm->var)->ident.value.value();
				if(cname == "iota") {
					return CTX_IOTA++;
				}
				if(cname == "reset") {
					int old = CTX_IOTA;
					CTX_IOTA = 0;
					return old;
				}
				std::optional<Constant> cns = const_lookup(cname);
				if(cns.has_value()) {
					return cns.value().value;
				}
			}
		}
		if(std::holds_alternative<NodeBinExpr*>(expr->var)) {
			NodeBinExpr* nbin = std::get<NodeBinExpr*>(expr->var);
			if(std::holds_alternative<NodeBinExprAdd*>(nbin->var)) {
				NodeBinExprAdd* nadd = std::get<NodeBinExprAdd*>(nbin->var);
				return eval_int_value(nadd->lhs) + eval_int_value(nadd->rhs);
			}
			if(std::holds_alternative<NodeBinExprSub*>(nbin->var)) {
				NodeBinExprSub* nsub = std::get<NodeBinExprSub*>(nbin->var);
				return eval_int_value(nsub->lhs) - eval_int_value(nsub->rhs);
			}
			if(std::holds_alternative<NodeBinExprMulti*>(nbin->var)) {
				NodeBinExprMulti* nmul = std::get<NodeBinExprMulti*>(nbin->var);
				return eval_int_value(nmul->lhs) * eval_int_value(nmul->rhs);
			}
			if(std::holds_alternative<NodeBinExprDiv*>(nbin->var)) {
				NodeBinExprDiv* ndiv = std::get<NodeBinExprDiv*>(nbin->var);
				return eval_int_value(ndiv->lhs) / eval_int_value(ndiv->rhs);
			}
			if(std::holds_alternative<NodeBinExprEqEq*>(nbin->var)) {
				NodeBinExprEqEq* neqeq = std::get<NodeBinExprEqEq*>(nbin->var);
				return eval_int_value(neqeq->lhs) == eval_int_value(neqeq->rhs);
			}
			if(std::holds_alternative<NodeBinExprNotEq*>(nbin->var)) {
				NodeBinExprNotEq* nnoteq = std::get<NodeBinExprNotEq*>(nbin->var);
				return eval_int_value(nnoteq->lhs) != eval_int_value(nnoteq->rhs);
			}
			if(std::holds_alternative<NodeBinExprLess*>(nbin->var)) {
				NodeBinExprLess* nless = std::get<NodeBinExprLess*>(nbin->var);
				return eval_int_value(nless->lhs) < eval_int_value(nless->rhs);
			}
			if(std::holds_alternative<NodeBinExprAbove*>(nbin->var)) {
				NodeBinExprAbove* nabove = std::get<NodeBinExprAbove*>(nbin->var);
				return eval_int_value(nabove->lhs) > eval_int_value(nabove->rhs);
			}
			if(std::holds_alternative<NodeBinExprAnd*>(nbin->var)) {
				NodeBinExprAnd* nand = std::get<NodeBinExprAnd*>(nbin->var);
				return eval_int_value(nand->lhs) > eval_int_value(nand->rhs);
			}
		}
		ParsingError("not constant provided");
		return result;
	}

	std::pair<NodeExpr*, size_t> parse_args() {
		NodeBinExprArgs* args = m_allocator.emplace<NodeBinExprArgs>();
		size_t size = 0;
		while(peek().has_value() && (peek().value().type != TokenType::close_paren && peek().value().type != TokenType::semi)) {
			auto expr = parse_expr();
			if(!expr.has_value()) {
				error_expected("expression");
			}
			if(peek().has_value() && (peek().value().type != TokenType::comma && peek().value().type != TokenType::semi && peek().value().type != TokenType::close_paren)) {
				error_expected(",");
			}
			if(peek().value().type == TokenType::close_paren) {
				args->args.push_back(expr.value());
				size += 1;
				break;
			}
			consume();
			args->args.push_back(expr.value());
			size += 1;
		}
		NodeBinExpr* bexpr = m_allocator.emplace<NodeBinExpr>();
		bexpr->var = args;
		NodeExpr* fexpr = m_allocator.emplace<NodeExpr>();
		fexpr->var = bexpr;
		std::pair<NodeExpr*, size_t> res = std::make_pair(fexpr, size);
		return res;
	}

	std::vector<std::vector<Token>*>* parse_macro_args() {
		std::vector<std::vector<Token>*>* __args = m_allocator.alloc<std::vector<std::vector<Token>*>>();
		__args->push_back(m_allocator.alloc<std::vector<Token>>());
		size_t nest_lvl = 0ULL;
		while(true) {
			if(peek().value().type == TokenType::open_paren) {
				nest_lvl += 1ULL;
				if(nest_lvl == 1) {
					consume();
					continue;
				}
			}
			else if(peek().value().type == TokenType::close_paren) {
				nest_lvl -= 1ULL;
				if(nest_lvl == 0) {
					break;
				}
			}
			Token cp = consume();
			if(cp.type == TokenType::comma && nest_lvl == 1) {
				__args->push_back(m_allocator.alloc<std::vector<Token>>());
			} else {
				__args->operator[](__args->size() - 1)->push_back(cp);
			}
		}
		consume();
		return __args;
	}

	void print_macro_args(std::vector<std::vector<Token>*>* __args) {
		for(int i = 0;i < static_cast<int>(__args->size());++i) {
			std::cout << "-------------\n";
			std::vector<Token>* L_args = __args->operator[](i);
			for(int j = 0;j < static_cast<int>(L_args->size());++j) {
				std::cout << L_args->operator[](j) << std::endl;
			}
			std::cout << "-------------\n";
		}
	}

	std::optional<size_t> __macro_arg_pos(Macro& __macro, std::string __arg) {
		for(int i = 0;i < static_cast<int>(__macro.args.size());++i) {
			if(__macro.args[i] == __arg) {
				return i;
			}
		}
		return std::nullopt;
	}

	void expand_macro(Macro& _macro, std::vector<std::vector<Token>*>* __args, Token __at) {
		if(_macro.args.size() != __args->size() && __args->size() != 1ULL) {
			ParsingError("macro `" + _macro.name + "` except " + std::to_string(_macro.args.size()) + " args, but got " + std::to_string(__args->size()));
		}
		std::vector<Token> body = _macro.body;
		for(int i = 0;i < static_cast<int>(body.size());++i) {
			body[i].line = __at.line;
			body[i].col = __at.col;
			body[i].file = __at.file;
			if(body[i].type == TokenType::ident) {
				std::optional<size_t> __arg = __macro_arg_pos(_macro, body[i].value.value());
				if(__arg.has_value()) {
					body.erase(body.begin() + i);
					body.insert(body.begin() + i, __args->operator[](__arg.value())->begin(), __args->operator[](__arg.value())->end());
				}
			}
		}
		m_tokens.insert(m_tokens.begin() + m_index, body.begin(), body.end());
	}

	void __expand_str(NodeTermStrLit* str) {
		while(peek().has_value() && peek().value().type == TokenType::string_lit) {
			str->str_lit.operator+=(consume().value.value());
		}
	}

	std::optional<NodeTerm*> parse_term() // NOLINT(*-no-recursion)
	{
		if (auto int_lit = try_consume(TokenType::int_lit)) {
			auto term_int_lit = m_allocator.emplace<NodeTermIntLit>(int_lit.value());
			auto term = m_allocator.emplace<NodeTerm>(term_int_lit);
			return term;
		}
		if(auto ampersand = try_consume(TokenType::ampersand)) {
			auto term_amp = m_allocator.emplace<NodeTermAmpersand>();
			if(auto term = parse_term()) {
				auto expr = m_allocator.emplace<NodeExpr>();
				expr->var = term.value();
				term_amp->expr = expr;
			} else {
				error_expected("expression");
			}
			auto term = m_allocator.emplace<NodeTerm>(term_amp);
			return term;
		}
		if (auto str_lit = try_consume(TokenType::string_lit)) {
			auto term_str_lit = m_allocator.emplace<NodeTermStrLit>(str_lit.value().value.value());
			__expand_str(term_str_lit);
			auto term = m_allocator.emplace<NodeTerm>(term_str_lit);
			return term;
		}
		if (auto rd8 = try_consume(TokenType::read8)) {
			auto term_rd8 = m_allocator.emplace<NodeTermRd>();
			try_consume_err(TokenType::open_paren);
			term_rd8->def = rd8.value();
			term_rd8->size = 8U;
			if(auto expr = parse_expr()) {
				term_rd8->expr = expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType::close_paren);
			auto term = m_allocator.emplace<NodeTerm>(term_rd8);
			return term;
		}
		if (auto cast = try_consume(TokenType::cast)) {
			auto term_cast = m_allocator.emplace<NodeTermCast>();
			try_consume_err(TokenType::open_paren);
			term_cast->def = cast.value();
			if(!is_type_token(peek().value().type)) {
				error_expected("type name");
			}
			Token type = consume();
			DataType dtype = uni_token_to_dt(type);
			try_consume_err(TokenType::comma);
			term_cast->type = dtype;
			if(auto expr = parse_expr()) {
				term_cast->expr = expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType::close_paren);
			auto term = m_allocator.emplace<NodeTerm>(term_cast);
			return term;
		}
		if (auto rd16 = try_consume(TokenType::read16)) {
			auto term_rd16 = m_allocator.emplace<NodeTermRd>();
			term_rd16->def = rd16.value();
			term_rd16->size = 16U;
			try_consume_err(TokenType::open_paren);
			if(auto expr = parse_expr()) {
				term_rd16->expr = expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType::close_paren);
			auto term = m_allocator.emplace<NodeTerm>(term_rd16);
			return term;
		}
		if (auto rd32 = try_consume(TokenType::read32)) {
			auto term_rd32 = m_allocator.emplace<NodeTermRd>();
			term_rd32->def = rd32.value();
			try_consume_err(TokenType::open_paren);
			term_rd32->size = 32U;
			if(auto expr = parse_expr()) {
				term_rd32->expr = expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType::close_paren);
			auto term = m_allocator.emplace<NodeTerm>(term_rd32);
			return term;
		}
		if(peek().has_value() && peek().value().type == TokenType::ident
			&& peek(1).has_value() && peek(1).value().type == TokenType::open_paren) {
			Token identif = try_consume_err(TokenType::ident);
			std::optional<Macro> __macro = macro_lookup(identif.value.value());
			if(__macro.has_value()) {
				std::vector<std::vector<Token>*>* __args = parse_macro_args();
				Macro _macro = __macro.value();
				expand_macro(_macro, __args, identif);
				return parse_term();
			}
			auto expr_call = m_allocator.emplace<NodeTermCall>();
			expr_call->def = identif;
			expr_call->name = identif.value.value();
			try_consume_err(TokenType::open_paren);
			if(peek().has_value() && peek().value().type == TokenType::close_paren) {
				expr_call->args = std::nullopt;
			} else {
				std::pair<NodeExpr*, size_t> pargs = parse_args();
				expr_call->args = pargs.first;
			}
			try_consume_err(TokenType::close_paren);
			auto stmt = m_allocator.emplace<NodeTerm>(expr_call);
			return stmt;
		}
		if (auto ident = try_consume(TokenType::ident)) {
			std::string tname = ident.value().value.value();
			auto expr_ident = m_allocator.emplace<NodeTermIdent>();
			expr_ident->ident = ident.value();
			auto term = m_allocator.emplace<NodeTerm>(expr_ident);
			return term;
		}
		if(auto _sizeof = try_consume(TokenType::_sizeof)) {
			try_consume_err(TokenType::open_paren);
			auto sizeof_term = m_allocator.emplace<NodeTermSizeof>();
			sizeof_term->def = _sizeof.value();
			Token ttype = consume();
			if(!is_type_token(ttype.type)) {
				error_expected("typename");
			}
			sizeof_term->type = uni_token_to_dt(ttype);
			try_consume_err(TokenType::close_paren);
			auto term = m_allocator.emplace<NodeTerm>(sizeof_term);
			return term;
		}
		if(auto _typeid = try_consume(TokenType::_typeid)) {
			auto typeid_term = m_allocator.emplace<NodeTermTypeid>();
			try_consume_err(TokenType::open_paren);
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
			try_consume_err(TokenType::close_paren);
			auto term = m_allocator.emplace<NodeTerm>(typeid_term);
			return term;
		}
		if (const auto open_paren = try_consume(TokenType::open_paren)) {
			auto expr = parse_expr();
			if (!expr.has_value()) {
				error_expected("expression");
			}
			try_consume_err(TokenType::close_paren);
			auto term_paren = m_allocator.emplace<NodeTermParen>(expr.value());
			auto term = m_allocator.emplace<NodeTerm>(term_paren);
			return term;
		}
		return {};
	}

	std::optional<NodeExpr*> parse_expr(const int min_prec = 0) // NOLINT(*-no-recursion)
	{
		std::optional<NodeTerm*> term_lhs = parse_term();
		if (!term_lhs.has_value()) {
			return {};
		}
		auto expr_lhs = m_allocator.emplace<NodeExpr>(term_lhs.value());
		if(peek().has_value() && peek().value().type == TokenType::dot) {
			Token def = consume();
			if(auto term = parse_term()) {
				NodeTerm* ident = term.value();
				NodeExpr* iexpr = m_allocator.emplace<NodeExpr>();
				iexpr->var = ident;
				NodeBinExprDot* dot = m_allocator.emplace<NodeBinExprDot>();
				dot->lhs = expr_lhs;
				dot->rhs = iexpr;
				NodeBinExpr* binexpr = m_allocator.emplace<NodeBinExpr>();
				binexpr->var = dot;
				NodeExpr* resval = m_allocator.emplace<NodeExpr>();
				resval->var = binexpr;
				expr_lhs = resval;
			} else {
				error_expected("identifier");
			}
		}
		while (true) {
			std::optional<Token> curr_tok = peek();
			std::optional<int> prec;
			if (curr_tok.has_value()) {
				prec = bin_prec(curr_tok->type);
				if (!prec.has_value() || prec < min_prec) {
					break;
				}
			}
			else {
				break;
			}
			const auto [type, line, col, value, file] = consume();
			Token ctok = {type, line, col, value, file};
			const int next_min_prec = prec.value() + 1;
			auto expr_rhs = parse_expr(next_min_prec);
			if (!expr_rhs.has_value()) {
				error_expected("expression");
			}
			auto expr = m_allocator.emplace<NodeBinExpr>();
			auto expr_lhs2 = m_allocator.emplace<NodeExpr>();
			if (type == TokenType::plus) {
				expr_lhs2->var = expr_lhs->var;
				auto add = m_allocator.emplace<NodeBinExprAdd>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = add;
			}
			else if (type == TokenType::star) {
				expr_lhs2->var = expr_lhs->var;
				auto multi = m_allocator.emplace<NodeBinExprMulti>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = multi;
			}
			else if (type == TokenType::minus) {
				expr_lhs2->var = expr_lhs->var;
				auto sub = m_allocator.emplace<NodeBinExprSub>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = sub;
			}
			else if (type == TokenType::fslash) {
				expr_lhs2->var = expr_lhs->var;
				auto div = m_allocator.emplace<NodeBinExprDiv>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = div;
			}
			else if (type == TokenType::mod) {
				expr_lhs2->var = expr_lhs->var;
				auto md = m_allocator.emplace<NodeBinExprMod>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = md;
			}
			else if (type == TokenType::eqeq) {
				expr_lhs2->var = expr_lhs->var;
				auto eqeq = m_allocator.emplace<NodeBinExprEqEq>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = eqeq;
			}
			else if (type == TokenType::_not_eq) {
				expr_lhs2->var = expr_lhs->var;
				auto nq = m_allocator.emplace<NodeBinExprNotEq>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = nq;
			}
			else if (type == TokenType::less) {
				expr_lhs2->var = expr_lhs->var;
				auto less = m_allocator.emplace<NodeBinExprLess>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = less;
			}
			else if (type == TokenType::above) {
				expr_lhs2->var = expr_lhs->var;
				auto above = m_allocator.emplace<NodeBinExprAbove>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = above;
			}
			else if (type == TokenType::dot) {
				expr_lhs2->var = expr_lhs->var;
				auto dot = m_allocator.emplace<NodeBinExprDot>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = dot;
			}
			else if (type == TokenType::double_ampersand) {
				expr_lhs2->var = expr_lhs->var;
				auto dot = m_allocator.emplace<NodeBinExprAnd>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = dot;
			}
			else if (type == TokenType::double_stick) {
				expr_lhs2->var = expr_lhs->var;
				auto dot = m_allocator.emplace<NodeBinExprOr>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = dot;
			}
			else if (type == TokenType::shift_left) {
				expr_lhs2->var = expr_lhs->var;
				auto dot = m_allocator.emplace<NodeBinExprShl>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = dot;
			}
			else if (type == TokenType::shift_right) {
				expr_lhs2->var = expr_lhs->var;
				auto dot = m_allocator.emplace<NodeBinExprShr>(expr_lhs2, expr_rhs.value());
				expr->def = ctok;
				expr->var = dot;
			}
			else {
				assert(false); // Unreachable;
			}
			expr_lhs->var = expr;
		}
		return expr_lhs;
	}

	std::optional<NodeScope*> parse_scope() // NOLINT(*-no-recursion)
	{
		if (!try_consume(TokenType::open_curly).has_value()) {
			return {};
		}
		auto scope = m_allocator.emplace<NodeScope>();
		while (true) {
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
		try_consume_err(TokenType::close_curly);
		return scope;
	}

	std::optional<NodeIfPred*> parse_if_pred() // NOLINT(*-no-recursion)
	{
		if (try_consume(TokenType::elif)) {
			try_consume_err(TokenType::open_paren);
			const auto elif = m_allocator.alloc<NodeIfPredElif>();
			if (const auto expr = parse_expr()) {
				elif->expr = expr.value();
			}
			else {
				error_expected("expression");
			}
			try_consume_err(TokenType::close_paren);
			if (const auto scope = parse_scope()) {
				elif->scope = scope.value();
			}
			else {
				error_expected("scope");
			}
			elif->pred = parse_if_pred();
			auto pred = m_allocator.emplace<NodeIfPred>(elif);
			return pred;
		}
		if (try_consume(TokenType::else_)) {
			auto else_ = m_allocator.alloc<NodeIfPredElse>();
			if (const auto scope = parse_scope()) {
				else_->scope = scope.value();
			}
			else {
				error_expected("scope");
			}
			auto pred = m_allocator.emplace<NodeIfPred>(else_);
			return pred;
		}
		return {};
	}

	std::optional<NodeStmt*> parse_stmt() // NOLINT(*-no-recursion)
	{
		if (peek().has_value() && peek().value().type == TokenType::exit && peek(1).has_value()
			&& peek(1).value().type == TokenType::open_paren) {
			consume();
			Token def = consume();
			auto stmt_exit = m_allocator.emplace<NodeStmtExit>();
			if (const auto node_expr = parse_expr()) {
				stmt_exit->expr = node_expr.value();
			}
			else {
				error_expected("expression");
			}
			stmt_exit->def = def;
			try_consume_err(TokenType::close_paren);
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_exit;
			return stmt;
		}
		if (peek().has_value() && peek().value().type == TokenType::let && peek(1).has_value()
			&& peek(1).value().type == TokenType::ident && peek(2).has_value()
			&& peek(2).value().type == TokenType::eq) {
			consume();
			auto stmt_let = m_allocator.emplace<NodeStmtLet>();
			stmt_let->ident = consume();
			consume();
			if (const auto expr = parse_expr()) {
				stmt_let->expr = expr.value();
			}
			else {
				error_expected("expression");
			}
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_let;
			return stmt;
		}
		if (peek().has_value() && peek().value().type == TokenType::let && peek(1).has_value()
			&& peek(1).value().type == TokenType::ident && peek(2).has_value()
			&& peek(2).value().type == TokenType::double_dot) {
			consume();
			auto stmt_let = m_allocator.emplace<NodeStmtLetNoAssign>();
			stmt_let->ident = consume();
			consume();
			if(!is_type_token(peek().value().type)) {
				error_expected("type");
			}
			DataType type = uni_token_to_dt(consume());
			stmt_let->type = type;
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_let;
			return stmt;
		}
		if (peek().has_value() && peek().value().type == TokenType::open_curly) {
			if (auto scope = parse_scope()) {
				auto stmt = m_allocator.emplace<NodeStmt>(scope.value());
				return stmt;
			}
			error_expected("scope");
		}
		if (auto if_ = try_consume(TokenType::if_)) {
			try_consume_err(TokenType::open_paren);
			auto stmt_if = m_allocator.emplace<NodeStmtIf>();
			if (const auto expr = parse_expr()) {
				stmt_if->expr = expr.value();
			}
			else {
				error_expected("expression");
			}
			try_consume_err(TokenType::close_paren);
			if (const auto scope = parse_scope()) {
				stmt_if->scope = scope.value();
			}
			else {
				ParsingError("after if except `{` and `}` after body", -1);
			}
			stmt_if->pred = parse_if_pred();
			auto stmt = m_allocator.emplace<NodeStmt>(stmt_if);
			return stmt;
		}
		if (auto while_ = try_consume(TokenType::wwhile)) {
			try_consume_err(TokenType::open_paren);
			auto stmt_while = m_allocator.emplace<NodeStmtWhile>();
			if (const auto expr = parse_expr()) {
				stmt_while->expr = expr.value();
			}
			else {
				error_expected("expression");
			}
			try_consume_err(TokenType::close_paren);
			if (const auto scope = parse_scope()) {
				stmt_while->scope = scope.value();
			}
			else {
				ParsingError("after while except `{` and `}` after body", -1);
			}
			auto stmt = m_allocator.emplace<NodeStmt>(stmt_while);
			return stmt;
		}
		if (peek().has_value() && peek().value().type == TokenType::proc) {
			consume();
			auto stmt_proc = m_allocator.emplace<NodeStmtProc>();
			Token identif = try_consume_err(TokenType::ident);
			std::vector<std::pair<std::string, DataType>> pparams;
			if(peek().has_value() && peek().value().type != TokenType::arrow) {
				for(int i = 0;peek().has_value() && peek().value().type != TokenType::arrow;++i) {
					Token argid = try_consume_err(TokenType::ident);
					try_consume_err(TokenType::double_dot);
					if(!is_type_token(peek().value().type)) {
						error_expected("arg type");
					}
					Token ttype = consume();
					DataType argtype = uni_token_to_dt(ttype);
					pparams.push_back(std::make_pair(argid.value.value(), argtype));
				}
			}
			try_consume_err(TokenType::arrow);
			if(!is_type_token(peek().value().type)) {
				error_expected("procedure return type");
			}
			DataType rettype = uni_token_to_dt(consume());
			stmt_proc->rettype = rettype;
			stmt_proc->name = identif.value.value();
			stmt_proc->params = pparams;
			stmt_proc->def = identif;
			if(auto open_b = try_consume(TokenType::open_bracket)) {
				while(peek().has_value() && peek().value().type != TokenType::close_bracket) {
					std::string attr_name = try_consume_err(TokenType::ident).value.value();
					std::optional<ProcAttr> cur_attr = string_to_PA(attr_name);
					if(!cur_attr.has_value()) {
						ParsingError("unkown Procedure Attribute `" + attr_name + "`");
					}
					stmt_proc->attrs.push_back(cur_attr.value());
				}
				try_consume_err(TokenType::close_bracket);
			}
			if(peek().has_value() && peek().value().type == TokenType::semi) {
				stmt_proc->scope = NULL;
				stmt_proc->prototype = true;
				auto stmt = m_allocator.emplace<NodeStmt>(stmt_proc);
				consume();
				return stmt;
			}
			if (const auto scope = parse_scope()) {
				stmt_proc->scope = scope.value();
			}
			else {
				ParsingError("after proc name except `{` and `}` after body", -1);
			}
			auto stmt = m_allocator.emplace<NodeStmt>(stmt_proc);
			return stmt;
		}
		if (peek().has_value() && peek().value().type == TokenType::ident &&
			peek(1).has_value() && peek(1).value().type == TokenType::open_paren) {
			Token identif = try_consume_err(TokenType::ident);
			std::optional<Macro> __macro = macro_lookup(identif.value.value());
			if(__macro.has_value()) {
				std::vector<std::vector<Token>*>* __args = parse_macro_args();
				Macro _macro = __macro.value();
				expand_macro(_macro, __args, identif);
				return parse_stmt();
			}
			auto stmt_call = m_allocator.emplace<NodeStmtCall>();
			stmt_call->def = identif;
			stmt_call->name = identif.value.value();
			try_consume_err(TokenType::open_paren);
			if(peek().has_value() && peek().value().type == TokenType::close_paren) {
				stmt_call->args = std::nullopt;
			} else {
				std::pair<NodeExpr*, size_t> pargs = parse_args();
				stmt_call->args = pargs.first;
			}
			try_consume_err(TokenType::close_paren);
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>(stmt_call);
			return stmt;
		}
		if(auto ret = try_consume(TokenType::_return)) {
			Token def = ret.value();
			auto stmt_return = m_allocator.emplace<NodeStmtReturn>();
			if(peek().has_value() && peek().value().type == TokenType::semi) {
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
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_return;
			return stmt;
		}
		if(auto store8 = try_consume(TokenType::store8)) {
			Token def = store8.value();
			auto stmt_st8 = m_allocator.emplace<NodeStmtStore>();
			stmt_st8->size = 8U;
			try_consume_err(TokenType::open_paren);
			std::pair<NodeExpr*, size_t> pargs = parse_args();
			if(pargs.second != 2U) {
				ParsingError("ptr, value");
			}
			NodeBinExprArgs* args = std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(pargs.first->var)->var);
			stmt_st8->ptr = args->args[0];
			stmt_st8->expr = args->args[1];
			stmt_st8->def = def;
			try_consume_err(TokenType::close_paren);
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_st8;
			return stmt;
		}
		if(auto store16 = try_consume(TokenType::store16)) {
			Token def = store16.value();
			auto stmt_st16 = m_allocator.emplace<NodeStmtStore>();
			stmt_st16->size = 16U;
			try_consume_err(TokenType::open_paren);
			std::pair<NodeExpr*, size_t> pargs = parse_args();
			if(pargs.second != 2U) {
				ParsingError("ptr, value");
			}
			NodeBinExprArgs* args = std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(pargs.first->var)->var);
			stmt_st16->ptr = args->args[0];
			stmt_st16->expr = args->args[1];
			stmt_st16->def = def;
			try_consume_err(TokenType::close_paren);
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_st16;
			return stmt;
		}
		if(auto store32 = try_consume(TokenType::store32)) {
			Token def = store32.value();
			auto stmt_st32 = m_allocator.emplace<NodeStmtStore>();
			stmt_st32->size = 32U;
			try_consume_err(TokenType::open_paren);
			std::pair<NodeExpr*, size_t> pargs = parse_args();
			if(pargs.second != 2U) {
				ParsingError("ptr, value");
			}
			NodeBinExprArgs* args = std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(pargs.first->var)->var);
			stmt_st32->ptr = args->args[0];
			stmt_st32->expr = args->args[1];
			stmt_st32->def = def;
			try_consume_err(TokenType::close_paren);
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_st32;
			return stmt;
		}

		if(auto inc = try_consume(TokenType::_include)) {
			if(peek().has_value() && peek().value().type != TokenType::string_lit) {
				error_expected("file path string");
			}
			std::string fname = consume().value.value() + ".bpm";
			std::string path = "";
			if(file_exists(fname)) {
				path = std::filesystem::canonical(fname).string();
			}
			else if(file_exists("lib/" + fname)) {
				path = std::filesystem::canonical("lib/" + fname).string();
			}
			else if(__slashinpath && file_exists(basepath + "/" + fname)) {
				path = std::filesystem::canonical(basepath + "/" + fname).string();
			}
			else {
				ParsingError("file not found at `include` - `" + fname + "`");
			}
			m_preprocessor_stmt = true;
			if(m_includes.find(path) != m_includes.end()) {
				return {};
			}
			std::string contents;
			{
				std::stringstream contents_stream;
				std::fstream input(path, std::ios::in);
				contents_stream << input.rdbuf();
				contents = contents_stream.str();
				input.close();
			}
			Tokenizer nlexer(std::move(contents));
			std::vector<Token> ntokens = nlexer.tokenize(fname);
			m_includes.insert(path);
			m_tokens.insert(m_tokens.begin() + m_index, ntokens.begin(), ntokens.end());
			return {};
		}

		if(auto buffer = try_consume(TokenType::buffer)) {
			Token def = buffer.value();
			auto stmt_buf = m_allocator.emplace<NodeStmtBuffer>();
			Token identif = try_consume_err(TokenType::ident);
			try_consume_err(TokenType::open_paren);
			stmt_buf->def = def;
			stmt_buf->name = identif.value.value();
			if(auto expr = parse_expr()) {
				stmt_buf->size = static_cast<size_t>(eval_int_value(expr.value()));
			} else {
				error_expected("constant expression");
			}
			try_consume_err(TokenType::close_paren);
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_buf;
			return stmt;
		}

		if(auto _asm = try_consume(TokenType::_asm)) {
			Token def = _asm.value();
			auto stmt_asm = m_allocator.emplace<NodeStmtAsm>();
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
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_asm;
			return stmt;
		}

		if(auto _cextern = try_consume(TokenType::cextern)) {
			Token def = _cextern.value();
			auto stmt_cextern = m_allocator.emplace<NodeStmtCextern>();
			stmt_cextern->name = try_consume_err(TokenType::string_lit).value.value();
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_cextern;
			return stmt;
		}

		if(auto _push = try_consume(TokenType::pushonstack)) {
			Token def = _push.value();
			auto stmt_push = m_allocator.emplace<NodeStmtPushOnStack>();
			stmt_push->def = def;
			try_consume_err(TokenType::open_paren);
			if(auto _expr = parse_expr()) {
				stmt_push->expr = _expr.value();
			} else {
				error_expected("expression");
			}
			try_consume_err(TokenType::close_paren);
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_push;
			return stmt;
		}

		if(auto _cns = try_consume(TokenType::_const)) {
			m_preprocessor_stmt = true;
			Token name = try_consume_err(TokenType::ident);
			if(auto expr = parse_expr()) {
				int _value = eval_int_value(expr.value());
				m_consts[name.value.value()] = { .name = name.value.value(), .value = _value};
			} else {
				error_expected("constant expression");
			}
			try_consume_err(TokenType::semi);
			return {};
		}

		if(auto _struct = try_consume(TokenType::_struct)) {
			Token def = _struct.value();
			auto stmt_struct = m_allocator.emplace<NodeStmtStruct>();
			stmt_struct->name = try_consume_err(TokenType::ident).value.value();
			stmt_struct->def = def;
			stmt_struct->__allocator = std::nullopt;
			if(peek().has_value() && peek().value().type == TokenType::open_paren) {
				consume();
				Token allc_id = try_consume_err(TokenType::ident);
				try_consume_err(TokenType::close_paren);
				stmt_struct->__allocator = allc_id.value.value();
			}
			try_consume_err(TokenType::open_curly);
			while(peek().has_value() && peek().value().type != TokenType::close_curly) {
				Token ident = try_consume_err(TokenType::ident);
				try_consume_err(TokenType::double_dot);
				if(!peek().has_value()) {
					error_expected("field type");
				}
				DataType dtype;
				if(!is_type_token(peek().value().type)) {
					error_expected("field type");
				}
				Token type = consume();
				dtype = uni_token_to_dt(type);
				stmt_struct->fields.push_back(std::make_pair(ident.value.value(), dtype));
				if(peek().has_value() && peek().value().type != TokenType::close_curly) {
					try_consume_err(TokenType::comma);
				}
			}
			try_consume_err(TokenType::close_curly);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_struct;
			return stmt;
		}

		if(auto _interface = try_consume(TokenType::interface)) {
			Token def = _interface.value();
			auto stmt_interface = m_allocator.emplace<NodeStmtInterface>();
			stmt_interface->name = try_consume_err(TokenType::ident).value.value();
			stmt_interface->def = def;
			try_consume_err(TokenType::open_curly);
			while(peek().has_value() && peek().value().type != TokenType::close_curly) {
				Token ident = try_consume_err(TokenType::ident);
				try_consume_err(TokenType::double_dot);
				if(!peek().has_value()) {
					error_expected("field type");
				}
				DataType dtype;
				if(!is_type_token(peek().value().type)) {
					error_expected("field type");
				}
				Token type = consume();
				dtype = uni_token_to_dt(type);
				stmt_interface->fields.push_back(std::make_pair(ident.value.value(), dtype));
				if(peek().has_value() && peek().value().type != TokenType::close_curly) {
					try_consume_err(TokenType::comma);
				}
			}
			try_consume_err(TokenType::close_curly);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_interface;
			return stmt;
		}

		if(auto _st_assert = try_consume(TokenType::_static_assert)) {
			m_preprocessor_stmt = true;
			try_consume_err(TokenType::open_paren);
			bool _static_condition = false;
			if(auto _expr = parse_expr()) {
				_static_condition = static_cast<bool>(eval_int_value(_expr.value()));
			} else {
				error_expected("expression");
			}

			try_consume_err(TokenType::comma);
			
			std::string _err_str = try_consume_err(TokenType::string_lit).value.value();
			
			try_consume_err(TokenType::close_paren);
			
			if(!_static_condition) {
				putloc(_st_assert.value());
				std::cerr << " Assertion Failed: " << _err_str << "\n";
				exit(1);
			}

			try_consume_err(TokenType::semi);
			
			return {};
		}

		if(auto del = try_consume(TokenType::_delete)) {
			Token def = del.value();
			auto stmt_delete = m_allocator.emplace<NodeStmtDelete>();
			if (const auto node_expr = parse_expr()) {
				stmt_delete->expr = node_expr.value();
			}
			else {
				error_expected("expression");
			}
			stmt_delete->def = def;
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_delete;
			return stmt;
		}

		if(auto _break = try_consume(TokenType::_break)) {
			Token def = _break.value();
			auto stmt_break = m_allocator.emplace<NodeStmtBreak>();
			stmt_break->def = def;
			try_consume_err(TokenType::semi);
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_break;
			return stmt;
		}

		if(auto _hash = try_consume(TokenType::hash_sign)) {
			Token _prep = consume();
			if(_prep.type == TokenType::_define) {
				m_preprocessor_stmt = true;
				std::string mname = try_consume_err(TokenType::ident).value.value();
				std::vector<std::string> __args;
				if(peek().has_value() && peek().value().type == TokenType::open_paren) {
					consume();
					while(peek().has_value() && peek().value().type != TokenType::close_paren) {
						std::string name = try_consume_err(TokenType::ident).value.value();
						__args.push_back(name);
					}
					consume();
				}
				Macro __macro = { .name = mname, .args = __args, .body = {} };
				while(peek().has_value() && peek().value().type != TokenType::dollar) {
					__macro.body.push_back(consume());
				}
				consume();
				m_macroses[mname] = __macro;

			}
			else {
				error_expected("preprocessor command");
			}
		}

		if(auto _oninit = try_consume(TokenType::oninit)) {
			auto stmt_oninit = m_allocator.emplace<NodeStmtOninit>();
			stmt_oninit->def = _oninit.value();
			if(auto _scope = parse_scope()) {
				stmt_oninit->scope = _scope.value();
			}
			else {
				error_expected("scope");
			}
			auto stmt = m_allocator.emplace<NodeStmt>();
			stmt->var = stmt_oninit;
			return stmt;
		}

		if(auto lvalue = parse_expr()) {
			if(!peek().has_value()) {
				error_expected("statement");
			}
			Token curtok = peek().value();		
			if(!lvalue.has_value()) {
				error_expected("lvalue");
			}
			if(curtok.type == TokenType::eq) {
				const auto stmt_assign = m_allocator.emplace<NodeStmtAssign>();
				stmt_assign->lvalue = lvalue.value();
				stmt_assign->def = consume();
				if (const auto expr = parse_expr()) {
					stmt_assign->expr = expr.value();
				}
				else {
					error_expected("expression");
				}
				try_consume_err(TokenType::semi);
				auto stmt = m_allocator.emplace<NodeStmt>(stmt_assign);
				return stmt;
			}
			else if(curtok.type == TokenType::plus_eq) {
				const auto stmt_assign = m_allocator.emplace<NodeStmtIncBy>();
				stmt_assign->lvalue = lvalue.value();
				stmt_assign->def = consume();
				if (const auto expr = parse_expr()) {
					stmt_assign->expr = expr.value();
				}
				else {
					error_expected("expression");
				}
				try_consume_err(TokenType::semi);
				auto stmt = m_allocator.emplace<NodeStmt>(stmt_assign);
				return stmt;
			}
			else if(curtok.type == TokenType::minus_eq) {
				const auto stmt_assign = m_allocator.emplace<NodeStmtDecBy>();
				stmt_assign->lvalue = lvalue.value();
				stmt_assign->def = consume();
				if (const auto expr = parse_expr()) {
					stmt_assign->expr = expr.value();
				}
				else {
					error_expected("expression");
				}
				try_consume_err(TokenType::semi);
				auto stmt = m_allocator.emplace<NodeStmt>(stmt_assign);
				return stmt;
			}
			else if(curtok.type == TokenType::star_eq) {
				const auto stmt_assign = m_allocator.emplace<NodeStmtMulBy>();
				stmt_assign->lvalue = lvalue.value();
				stmt_assign->def = consume();
				if (const auto expr = parse_expr()) {
					stmt_assign->expr = expr.value();
				}
				else {
					error_expected("expression");
				}
				try_consume_err(TokenType::semi);
				auto stmt = m_allocator.emplace<NodeStmt>(stmt_assign);
				return stmt;
			}
			else if(curtok.type == TokenType::fslash_eq) {
				const auto stmt_assign = m_allocator.emplace<NodeStmtDivBy>();
				stmt_assign->lvalue = lvalue.value();
				stmt_assign->def = consume();
				if (const auto expr = parse_expr()) {
					stmt_assign->expr = expr.value();
				}
				else {
					error_expected("expression");
				}
				try_consume_err(TokenType::semi);
				auto stmt = m_allocator.emplace<NodeStmt>(stmt_assign);
				return stmt;
			}
			else {
				error_expected("statement");
			}
		}

		return {};
	}

	std::optional<NodeProg> parse_prog()
	{
		NodeProg prog;
		while (peek().has_value()) {
			if (auto stmt = parse_stmt()) {
				prog.stmts.push_back(stmt.value());
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

	std::unordered_map<std::string, Constant>* get_consts() {
		return &m_consts;
	}

private:
	[[nodiscard]] std::optional<Token> peek(const int offset = 0) const
	{
		if (m_index + offset >= m_tokens.size()) {
			return {};
		}
		return m_tokens.at(m_index + offset);
	}

	Token consume()
	{
		return m_tokens.at(m_index++);
	}

	Token try_consume_err(const TokenType type)
	{
		if (peek().has_value() && peek().value().type == type) {
			return consume();
		}
		error_expected(tok_to_string(type));
		return {};
	}

	std::optional<Token> try_consume(const TokenType type)
	{
		if (peek().has_value() && peek().value().type == type) {
			return consume();
		}
		return {};
	}

	std::vector<Token>	   m_tokens;
	std::unordered_set<std::string> m_includes;
	std::unordered_map<std::string, Constant> m_consts;
	std::unordered_map<std::string, Macro> m_macroses;
	bool m_preprocessor_stmt = false;
	size_t m_index = 0ULL;
	size_t CTX_IOTA = 0ULL;
	ArenaAllocator m_allocator;
};