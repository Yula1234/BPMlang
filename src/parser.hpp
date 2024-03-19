#pragma once

#include <cassert>
#include <variant>
#include <filesystem>

#include "arena.hpp"
#include "tokenization.hpp"

enum class DataType {
    _int,
    ptr,
    _void,
};

#define yforeach(container) for(int i = 0;i < static_cast<int>(container.size());++i)

std::string dt_to_string(DataType dt) {
    switch(dt) {
    case DataType::_int:
        return "`int`";
    case DataType::ptr:
        return "`ptr`";
    case DataType::_void:
        return "`void`";
    default:
        break;
    }
    assert(false);
}

DataType token_to_dt(TokenType tt) {
    switch(tt) {
    case TokenType::int_type:
        return DataType::_int;
    case TokenType::ptr_type:
        return DataType::ptr;
    case TokenType::void_type:
        return DataType::_void;
    default:
        break;
    }
    assert(false); // unreacheable
}

std::ostream& operator<<(std::ostream& out, const DataType dt) {
    std::cout << dt_to_string(dt); 
    return out;
}

enum class ProcAttr {
    nostdargs,
    noprolog,
};

std::optional<ProcAttr> string_to_PA(std::string str) {
    if(str == "nostdargs") {
        return ProcAttr::nostdargs;
    }
    if(str == "noprolog") {
        return ProcAttr::noprolog;
    }
    return std::nullopt;
}

struct NodeTermIntLit {
    Token int_lit;
};

struct NodeTermStrLit {
    Token str_lit;
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

struct NodeTermRd {
    Token def;
    size_t size;
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

struct NodeBinExprArgs {
    std::vector<NodeExpr*> args;
};

struct NodeBinExpr {
    Token def;
    std::variant<NodeBinExprAdd*, NodeBinExprMulti*, NodeBinExprSub*, NodeBinExprDiv*, NodeBinExprEqEq*, NodeBinExprLess*, NodeBinExprAbove*, NodeBinExprArgs*, NodeBinExprNotEq*, NodeBinExprMod*> var;
};

struct NodeTerm {
    std::variant<NodeTermIntLit*, NodeTermStrLit*, NodeTermIdent*, NodeTermParen*, NodeTermCall*, NodeTermRd*, NodeTermAmpersand*> var;
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
    NodeExpr* expr;
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

struct NodeStmt {
    std::variant<NodeStmtExit*, NodeStmtLet*,
                NodeScope*, NodeStmtIf*,
                NodeStmtAssign*,NodeStmtAsm*,
                NodeStmtProc*, NodeStmtCall*,
                NodeStmtWhile*,NodeStmtReturn*,
                NodeStmtStore*,NodeStmtBuffer*,
                NodeStmtCextern*> var;
};

struct NodeProg {
    std::vector<NodeStmt*> stmts {};
};

class Parser {
public:
    struct Constant {
        std::string name;
        int value;
    };

    explicit Parser(std::vector<Token> tokens)
        : m_tokens(std::move(tokens))
        , m_allocator(1024 * 1024 * 24) // 24 mb
    {
    }

    std::optional<Constant> const_lookup(std::string name) {
        Constant it;
        yforeach(m_consts) {
            if(m_consts[i].name == name) {
                return m_consts[i];
            }
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
        std::cout << " ERROR: excepted " << msg << ", but got " << tok_to_string(peek().value().type) << "\n";
        exit(EXIT_FAILURE);
    }

    int eval_int_value(NodeExpr* expr) {
        int result = 0;
        if(std::holds_alternative<NodeTerm*>(expr->var)) {
            NodeTerm* nterm = std::get<NodeTerm*>(expr->var);
            if(std::holds_alternative<NodeTermIntLit*>(nterm->var)) {
                return std::stoi(std::get<NodeTermIntLit*>(nterm->var)->int_lit.value.value());
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
        }
        ParsingError("not constant provided");
        return result;
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
            auto term_str_lit = m_allocator.emplace<NodeTermStrLit>(str_lit.value());
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
            auto expr_call = m_allocator.emplace<NodeTermCall>();
            Token identif = try_consume_err(TokenType::ident);
            expr_call->def = identif;
            expr_call->name = identif.value.value();
            if(std::find(m_used_procedures.begin(), m_used_procedures.end(), expr_call->name) == m_used_procedures.end()) {
                m_used_procedures.push_back(expr_call->name);
            }
            if(peek(1).has_value() && peek(1).value().type == TokenType::close_paren) {
                consume();
                consume();
                expr_call->args = std::nullopt;
            } else {
                expr_call->args = parse_expr();
            }
            auto stmt = m_allocator.emplace<NodeTerm>(expr_call);
            return stmt;
        }
        if (auto ident = try_consume(TokenType::ident)) {
            std::string tname = ident.value().value.value();
            auto expr_ident = m_allocator.emplace<NodeTermIdent>();
            std::optional<Constant> cns = const_lookup(tname);
            if(!cns.has_value()) {
                expr_ident->ident = ident.value();
                auto term = m_allocator.emplace<NodeTerm>(expr_ident);
                return term;
            } else {
                auto CnsTerm = m_allocator.emplace<NodeTermIntLit>();
                CnsTerm->int_lit = { .type = TokenType::int_lit, .line = 0, .col = 0, .value = std::to_string(cns.value().value), .file = ""};
                auto term = m_allocator.emplace<NodeTerm>(CnsTerm);
                return term;
            }
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
            else if (type == TokenType::comma) {
                expr_lhs2->var = expr_lhs->var;
                NodeExpr* left = expr_lhs2;
                NodeExpr* right = expr_rhs.value();
                std::vector<NodeExpr*> args;
                if(std::holds_alternative<NodeBinExpr*>(left->var)) {
                    if(std::holds_alternative<NodeBinExprArgs*>(std::get<NodeBinExpr*>(left->var)->var)) {
                        std::vector<NodeExpr*> largs = std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(left->var)->var)->args;
                        for(int i = 0;i < static_cast<int>(largs.size());++i) {
                            args.push_back(largs[i]);
                        }
                    }
                } else {
                    args.push_back(left);
                }
                if(std::holds_alternative<NodeBinExpr*>(right->var)) {
                    if(std::holds_alternative<NodeBinExprArgs*>(std::get<NodeBinExpr*>(right->var)->var)) {
                        std::vector<NodeExpr*> rargs = std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(right->var)->var)->args;
                        for(int i = 0;i < static_cast<int>(rargs.size());++i) {
                            args.push_back(rargs[i]);
                        }
                    }
                } else {
                    args.push_back(right);
                }
                auto argsl = m_allocator.emplace<NodeBinExprArgs>();
                argsl->args = args;
                expr->def = ctok;
                expr->var = argsl;
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
        while (auto stmt = parse_stmt()) {
            scope->stmts.push_back(stmt.value());
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
        if (peek(1).has_value()
            && peek(1).value().type == TokenType::eq) {
            const auto assign = m_allocator.emplace<NodeStmtAssign>();
            if(auto expr = parse_expr()) {
                assign->lvalue = expr.value();
            } else {
                error_expected("lvalue");
            }
            assign->def = try_consume_err(TokenType::eq);
            if (const auto expr = parse_expr()) {
                assign->expr = expr.value();
            }
            else {
                error_expected("expression");
            }
            try_consume_err(TokenType::semi);
            auto stmt = m_allocator.emplace<NodeStmt>(assign);
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
                    if(peek().value().type != TokenType::int_type && peek().value().type != TokenType::ptr_type) {
                        error_expected("type");
                    }
                    Token ttype = consume();
                    DataType argtype = token_to_dt(ttype.type);
                    pparams.push_back(std::make_pair(argid.value.value(), argtype));
                }
            }
            try_consume_err(TokenType::arrow);
            if(peek().has_value() && peek().value().type != TokenType::int_type && peek().value().type != TokenType::ptr_type && peek().value().type != TokenType::void_type) {
                error_expected("procedure return type");
            }
            DataType rettype = token_to_dt(consume().type);
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
            auto stmt_call = m_allocator.emplace<NodeStmtCall>();
            Token identif = try_consume_err(TokenType::ident);
            stmt_call->def = identif;
            stmt_call->name = identif.value.value();
            if(std::find(m_used_procedures.begin(), m_used_procedures.end(), stmt_call->name) == m_used_procedures.end()) {
                m_used_procedures.push_back(stmt_call->name);
            }
            if(peek(1).has_value() && peek(1).value().type == TokenType::close_paren) {
                consume();
                consume();
                stmt_call->args = std::nullopt;
            } else {
                stmt_call->args = parse_expr();
            }
            try_consume_err(TokenType::semi);
            auto stmt = m_allocator.emplace<NodeStmt>(stmt_call);
            return stmt;
        }
        if(auto ret = try_consume(TokenType::_return)) {
            Token def = ret.value();
            auto stmt_return = m_allocator.emplace<NodeStmtReturn>();
            if (const auto node_expr = parse_expr()) {
                stmt_return->expr = node_expr.value();
            }
            else {
                error_expected("expression");
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
            if (const auto node_expr = parse_expr()) {
                NodeExpr* expr = node_expr.value();
                if(!std::holds_alternative<NodeBinExpr*>(expr->var)) {
                    error_expected("ptr, value");
                }
                if(!std::holds_alternative<NodeBinExprArgs*>(std::get<NodeBinExpr*>(expr->var)->var)) {
                    error_expected("ptr, value");
                }
                NodeBinExprArgs* args = std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(expr->var)->var);
                std::vector<NodeExpr*> vargs = args->args;
                if(vargs.size() != 2U) {
                    error_expected("ptr, value");
                }
                stmt_st8->ptr = vargs[0];
                stmt_st8->expr = vargs[1];
            }
            else {
                error_expected("expression");
            }
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
            if (const auto node_expr = parse_expr()) {
                NodeExpr* expr = node_expr.value();
                if(!std::holds_alternative<NodeBinExpr*>(expr->var)) {
                    error_expected("ptr, value");
                }
                if(!std::holds_alternative<NodeBinExprArgs*>(std::get<NodeBinExpr*>(expr->var)->var)) {
                    error_expected("ptr, value");
                }
                NodeBinExprArgs* args = std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(expr->var)->var);
                std::vector<NodeExpr*> vargs = args->args;
                if(vargs.size() != 2U) {
                    error_expected("ptr, value");
                }
                stmt_st16->ptr = vargs[0];
                stmt_st16->expr = vargs[1];
            }
            else {
                error_expected("expression");
            }
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
            if (const auto node_expr = parse_expr()) {
                NodeExpr* expr = node_expr.value();
                if(!std::holds_alternative<NodeBinExpr*>(expr->var)) {
                    error_expected("ptr, value");
                }
                if(!std::holds_alternative<NodeBinExprArgs*>(std::get<NodeBinExpr*>(expr->var)->var)) {
                    error_expected("ptr, value");
                }
                NodeBinExprArgs* args = std::get<NodeBinExprArgs*>(std::get<NodeBinExpr*>(expr->var)->var);
                std::vector<NodeExpr*> vargs = args->args;
                if(vargs.size() != 2U) {
                    error_expected("ptr, value");
                }
                stmt_st32->ptr = vargs[0];
                stmt_st32->expr = vargs[1];
            }
            else {
                error_expected("expression");
            }
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
            std::string fname = "./lib/" + consume().value.value() + ".bpm";
            std::string path = std::filesystem::canonical(fname).string();
            m_proprocessor_stmt = true;
            if(std::find(m_includes.begin(), m_includes.end(), path) != m_includes.end()) {
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
            m_includes.push_back(path);
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
            stmt_asm->code = try_consume_err(TokenType::string_lit).value.value();
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

        if(auto _cns = try_consume(TokenType::_const)) {
            m_proprocessor_stmt = true;
            Token name = try_consume_err(TokenType::ident);
            if(auto expr = parse_expr()) {
                int _value = eval_int_value(expr.value());
                m_consts.push_back({ .name = name.value.value(), .value = _value});
            } else {
                error_expected("constant expression");
            }
            try_consume_err(TokenType::semi);
            return {};
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
                if(m_proprocessor_stmt) {
                    m_proprocessor_stmt = false;
                } else {
                    error_expected("statement");
                }
            }
        }
        return prog;
    }

    std::vector<std::string>& get_used() {
        return m_used_procedures;
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

    std::vector<Token> m_tokens;
    std::vector<std::string> m_includes;
    std::vector<Constant> m_consts;
    std::vector<std::string> m_used_procedures {
        "main"
    };
    bool m_proprocessor_stmt = false;
    size_t m_index = 0;
    ArenaAllocator m_allocator;
};