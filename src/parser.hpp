#pragma once

#include <cassert>
#include <variant>

#include "arena.hpp"
#include "tokenization.hpp"

enum class DataType {
    _int,
    string,
};

std::string dt_to_string(DataType dt) {
    switch(dt) {
    case DataType::_int:
        return "`int`";
    case DataType::string:
        return "`string`";
    }
    assert(false);
}

std::ostream& operator<<(std::ostream& out, const DataType dt) {
    std::cout << dt_to_string(dt); 
    return out;
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

struct NodeTermParen {
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

struct NodeBinExprArgs {
    std::vector<NodeExpr*> args;
};

struct NodeBinExpr {
    Token def;
    std::variant<NodeBinExprAdd*, NodeBinExprMulti*, NodeBinExprSub*, NodeBinExprDiv*, NodeBinExprEqEq*, NodeBinExprLess*, NodeBinExprAbove*, NodeBinExprArgs*> var;
};

struct NodeTerm {
    std::variant<NodeTermIntLit*, NodeTermStrLit*, NodeTermIdent*, NodeTermParen*> var;
};

struct NodeExpr {
    std::variant<NodeTerm*, NodeBinExpr*> var;
};

struct NodeStmtExit {
    Token def;
    NodeExpr* expr;
};

struct NodeStmtPrint {
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
    Token ident;
    NodeExpr* expr {};
};

struct NodeStmtProc {
    std::string name;
    std::vector<DataType> args;
    NodeScope* scope {};
};

struct NodeStmtCall {
    Token def;
    std::string name;
    std::optional<NodeExpr*> args;
};

struct NodeStmt {
    std::variant<NodeStmtExit*, NodeStmtLet*,
                NodeScope*, NodeStmtIf*,
                NodeStmtAssign*, NodeStmtPrint*,
                NodeStmtProc*, NodeStmtCall*> var;
};

struct NodeProg {
    std::vector<NodeStmt*> stmts;
};

class Parser {
public:
    explicit Parser(std::vector<Token> tokens)
        : m_tokens(std::move(tokens))
        , m_allocator(1024 * 1024 * 24) // 24 mb
    {
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

    std::optional<NodeTerm*> parse_term() // NOLINT(*-no-recursion)
    {
        if (auto int_lit = try_consume(TokenType::int_lit)) {
            auto term_int_lit = m_allocator.emplace<NodeTermIntLit>(int_lit.value());
            auto term = m_allocator.emplace<NodeTerm>(term_int_lit);
            return term;
        }
        if (auto str_lit = try_consume(TokenType::string_lit)) {
            auto term_str_lit = m_allocator.emplace<NodeTermStrLit>(str_lit.value());
            auto term = m_allocator.emplace<NodeTerm>(term_str_lit);
            return term;
        }
        if(peek().has_value() && peek().value().type == TokenType::ident
            && peek(1).has_value() && peek(1).value().type == TokenType::open_paren) {
            auto ident = consume(); // fname
            std::cout << "at parsing function call\n";
            exit(0);
        }
        if (auto ident = try_consume(TokenType::ident)) {
            auto expr_ident = m_allocator.emplace<NodeTermIdent>(ident.value());
            auto term = m_allocator.emplace<NodeTerm>(expr_ident);
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
            else if (type == TokenType::eqeq) {
                expr_lhs2->var = expr_lhs->var;
                auto eqeq = m_allocator.emplace<NodeBinExprEqEq>(expr_lhs2, expr_rhs.value());
                expr->def = ctok;
                expr->var = eqeq;
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
        if (peek().has_value() && peek().value().type == TokenType::print && peek(1).has_value()
            && peek(1).value().type == TokenType::open_paren) {
            consume();
            Token def = consume();
            auto stmt_print = m_allocator.emplace<NodeStmtPrint>();
            if (const auto node_expr = parse_expr()) {
                stmt_print->expr = node_expr.value();
            }
            else {
                error_expected("expression");
            }
            stmt_print->def = def;
            try_consume_err(TokenType::close_paren);
            try_consume_err(TokenType::semi);
            auto stmt = m_allocator.emplace<NodeStmt>();
            stmt->var = stmt_print;
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
        if (peek().has_value() && peek().value().type == TokenType::ident && peek(1).has_value()
            && peek(1).value().type == TokenType::eq) {
            const auto assign = m_allocator.emplace<NodeStmtAssign>();
            assign->ident = consume();
            consume();
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
        if (peek().has_value() && peek().value().type == TokenType::proc) {
            consume();
            auto stmt_proc = m_allocator.emplace<NodeStmtProc>();
            Token identif = try_consume_err(TokenType::ident);
            stmt_proc->name = identif.value.value();
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
                error_expected("statement");
            }
        }
        return prog;
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

    const std::vector<Token> m_tokens;
    size_t m_index = 0;
    ArenaAllocator m_allocator;
};