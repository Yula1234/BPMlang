#pragma once

#include <string>
#include <vector>
#include <cstdio>

enum class TokenType {
    exit,
    int_lit,
    semi,
    open_paren,
    close_paren,
    ident,
    let,
    eqeq,
    eq,
    plus,
    star,
    minus,
    fslash,
    open_curly,
    close_curly,
    if_,
    elif,
    else_,
    print,
    proc,
    in,
    string_lit,
};

std::string tok_to_string(const TokenType type)
{
    switch (type) {
    case TokenType::exit:
        return "`exit`";
    case TokenType::int_lit:
        return "`int literal`";
    case TokenType::semi:
        return "`;`";
    case TokenType::open_paren:
        return "`(`";
    case TokenType::close_paren:
        return "`)`";
    case TokenType::ident:
        return "identifier";
    case TokenType::let:
        return "`let`";
    case TokenType::eq:
        return "`=`";
    case TokenType::eqeq:
        return "`==`";
    case TokenType::plus:
        return "`+`";
    case TokenType::star:
        return "`*`";
    case TokenType::minus:
        return "`-`";
    case TokenType::fslash:
        return "`/`";
    case TokenType::open_curly:
        return "`{`";
    case TokenType::close_curly:
        return "`}`";
    case TokenType::if_:
        return "`if`";
    case TokenType::elif:
        return "`elif`";
    case TokenType::else_:
        return "`else`";
    case TokenType::print:
        return "`print`";
    case TokenType::proc:
        return "`proc`";
    case TokenType::in:
        return "`in`";
    case TokenType::string_lit:
        return "`string literal`";
    }
    assert(false);
}

int prec_IOTA = 0;
inline std::optional<int> bin_prec(const TokenType type)
{
    switch (type) {
    case TokenType::eqeq:
        return prec_IOTA++;
    case TokenType::minus:
    case TokenType::plus:
        return prec_IOTA++;
    case TokenType::fslash:
    case TokenType::star:
        return prec_IOTA++;
    default:
        return {};
    }
}

struct Token {
    TokenType type;
    int line;
    int col;
    std::optional<std::string> value {};
    std::string file;
};

void putloc(Token tok) {
    printf("%s %d:%d", tok.file.c_str(), tok.line, tok.col);
}

std::string loc_of(Token tok) {
    static char buffer[2048];
    sprintf(buffer, "%s %d:%d", tok.file.c_str(), tok.line, tok.col);
    std::string str(buffer);
    return str;
}

class Tokenizer {
public:
    explicit Tokenizer(std::string src)
        : m_src(std::move(src))
    {
    }

    std::vector<Token> tokenize(std::string file)
    {
        std::vector<Token> tokens;
        std::string buf;
        int line_count = 1;
        while (peek().has_value()) {
            if (std::isalpha(peek().value())) {
                buf.push_back(consume());
                while (peek().has_value() && std::isalnum(peek().value())) {
                    buf.push_back(consume());
                }
                if (buf == "exit") {
                    tokens.push_back({ TokenType::exit, line_count, m_col - (int)buf.size(), file });
                    buf.clear();
                }
                else if (buf == "let") {
                    tokens.push_back({ TokenType::let, line_count, m_col - (int)buf.size(), file });
                    buf.clear();
                }
                else if (buf == "if") {
                    tokens.push_back({ TokenType::if_, line_count, m_col - (int)buf.size(), file });
                    buf.clear();
                }
                else if (buf == "elif") {
                    tokens.push_back({ TokenType::elif, line_count, m_col - (int)buf.size(), file });
                    buf.clear();
                }
                else if (buf == "else") {
                    tokens.push_back({ TokenType::else_, line_count, m_col - (int)buf.size(), file });
                    buf.clear();
                }
                else if (buf == "print") {
                    tokens.push_back({ TokenType::print, line_count, m_col - (int)buf.size(), file });
                    buf.clear();
                }
                else if (buf == "proc") {
                    tokens.push_back({ TokenType::proc, line_count, m_col - (int)buf.size(), file });
                    buf.clear();
                }
                else {
                    tokens.push_back({ TokenType::ident, line_count, m_col - (int)buf.size(), buf, file });
                    buf.clear();
                }
            }
            else if (std::isdigit(peek().value())) {
                buf.push_back(consume());
                while (peek().has_value() && std::isdigit(peek().value())) {
                    buf.push_back(consume());
                }
                tokens.push_back({ .type = TokenType::int_lit, .line = line_count, .col = m_col - (int)buf.size(), .value = buf, .file = file });
                buf.clear();
            }
            else if (peek().value() == '/' && peek(1).has_value() && peek(1).value() == '/') {
                consume();
                consume();
                while (peek().has_value() && peek().value() != '\n') {
                    consume();
                }
            }
            else if (peek().value() == '/' && peek(1).has_value() && peek(1).value() == '*') {
                consume();
                consume();
                while (peek().has_value()) {
                    if (peek().value() == '*' && peek(1).has_value() && peek(1).value() == '/') {
                        break;
                    }
                    consume();
                }
                if (peek().has_value()) {
                    consume();
                }
                if (peek().has_value()) {
                    consume();
                }
            }
            else if(peek().value() == '"') {
                consume();
                buf.clear();
                while(peek().has_value() && peek().value() != '"') {
                    buf.push_back(consume());
                }
                consume();
                for(int i = 0;i < (int)buf.size();++i) {
                    if(buf[i] == '\\') {
                        if(buf[i+1] == 'n') {
                            buf.erase(buf.begin()+i);
                            buf[i] = '\n';
                        }
                    }
                }
                tokens.push_back({ .type = TokenType::string_lit, .line = line_count , .col = m_col - (int)buf.size(), .value = buf, .file = file });
                buf.clear();
            }
            else if (peek().value() == '(') {
                consume();
                tokens.push_back({ .type = TokenType::open_paren, .line =  line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == ')') {
                consume();
                tokens.push_back({ .type = TokenType::close_paren, .line =  line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == ';') {
                consume();
                tokens.push_back({ .type = TokenType::semi, .line =  line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '=' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens.push_back({ .type = TokenType::eqeq, .line = line_count, .col = m_col - 2, .file = file });
            }
            else if (peek().value() == '=') {
                consume();
                tokens.push_back({ .type = TokenType::eq, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '+') {
                consume();
                tokens.push_back({ .type = TokenType::plus, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '*') {
                consume();
                tokens.push_back({ .type = TokenType::star, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '-') {
                consume();
                tokens.push_back({ .type = TokenType::minus, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '/') {
                consume();
                tokens.push_back({ .type = TokenType::fslash, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '{') {
                consume();
                tokens.push_back({ .type = TokenType::open_curly, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '}') {
                consume();
                tokens.push_back({ .type = TokenType::close_curly, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '\n') {
                consume();
                m_col = 1;
                line_count++;
            }
            else if (std::isspace(peek().value())) {
                consume();
            }
            else {
                std::cerr << "Invalid token" << std::endl;
                exit(EXIT_FAILURE);
            }
        }
        m_index = 0;
        return tokens;
    }

private:
    [[nodiscard]] std::optional<char> peek(const size_t offset = 0) const
    {
        if (m_index + offset >= m_src.length()) {
            return {};
        }
        return m_src.at(m_index + offset);
    }

    char consume()
    {
        m_col++;
        return m_src.at(m_index++);
    }

    const std::string m_src;
    size_t m_index = 0;
    int m_col = 1;
};