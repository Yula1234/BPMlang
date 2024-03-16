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
    less,
    eqeq,
    eq,
    plus,
    star,
    minus,
    fslash,
    above,
    open_curly,
    close_curly,
    if_,
    elif,
    else_,
    print,
    proc,
    in,
    string_lit,
    comma,
    int_type,
    string_type,
    double_dot,
    wwhile,
};

#define BinaryOpsCount 7
#define StmtsCount 7

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
    case TokenType::less:
        return "`<`";
    case TokenType::above:
        return "`>`";
    case TokenType::comma:
        return "`,`";
    case TokenType::int_type:
        return "`int`";
    case TokenType::string_type:
        return "`string`";
    case TokenType::double_dot:
        return "`:`";
    case TokenType::wwhile:
        return "`while`";
    }
    assert(false);
}

int prec_IOTA = 0;
inline std::optional<int> bin_prec(const TokenType type)
{
    switch (type) {
    case TokenType::comma:
        return prec_IOTA++;
    case TokenType::eqeq:
    case TokenType::less:
    case TokenType::above:
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
    friend std::ostream& operator<<(std::ostream& out, const Token& tok) {
        out << "Token(.type = " << tok_to_string(tok.type);
        out << ", .line = " << tok.line;
        out << ", .col = " << tok.col;
        if(tok.value.has_value()) {
            out << ", .value = " << tok.value.value();
        }
        out << ")";
        return out;
    }
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

bool is_valid_id(char c) {
    switch(c) {
    case '_':
        return true;
    default:
        return false;
    }
    return false;
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
            if (std::isalpha(peek().value()) || is_valid_id(peek().value())) {
                buf.push_back(consume());
                while (peek().has_value() && (std::isalnum(peek().value()) || is_valid_id(peek().value()))) {
                    buf.push_back(consume());
                }
                if (buf == "exit") {
                    tokens.push_back({ .type = TokenType::exit, .line =  line_count, .col =  m_col - (int)buf.size(), .file = file });
                    buf.clear();
                }
                else if (buf == "let") {
                    tokens.push_back({ .type = TokenType::let, .line =  line_count, .col =  m_col - (int)buf.size(), .file = file });
                    buf.clear();
                }
                else if (buf == "if") {
                    tokens.push_back({ .type = TokenType::if_, .line =  line_count, .col =  m_col - (int)buf.size(), .file = file });
                    buf.clear();
                }
                else if (buf == "elif") {
                    tokens.push_back({ .type = TokenType::elif, .line =  line_count, .col =  m_col - (int)buf.size(), .file = file });
                    buf.clear();
                }
                else if (buf == "else") {
                    tokens.push_back({ .type = TokenType::else_, .line =  line_count, .col =  m_col - (int)buf.size(), .file = file });
                    buf.clear();
                }
                else if (buf == "print") {
                    tokens.push_back({ .type = TokenType::print, .line =  line_count, .col =  m_col - (int)buf.size(), .file = file });
                    buf.clear();
                }
                else if (buf == "proc") {
                    tokens.push_back({ .type = TokenType::proc, .line =  line_count, .col =  m_col - (int)buf.size(), .file = file });
                    buf.clear();
                }
                else if (buf == "int") {
                    tokens.push_back({ .type = TokenType::int_type, .line =  line_count, .col =  m_col - (int)buf.size(), .file = file });
                    buf.clear();
                }
                else if (buf == "string") {
                    tokens.push_back({ .type = TokenType::string_type, .line =  line_count, .col =  m_col - (int)buf.size(), .file = file });
                    buf.clear();
                }
                else if (buf == "while") {
                    tokens.push_back({ .type = TokenType::wwhile, .line =  line_count, .col =  m_col - (int)buf.size(), .file = file });
                    buf.clear();
                }
                else {
                    tokens.push_back({ .type = TokenType::ident, .line =  line_count, .col =  m_col - (int)buf.size(), .value = buf, .file = file });
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
            else if (peek().value() == ',') {
                consume();
                tokens.push_back({ .type = TokenType::comma, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == ':') {
                consume();
                tokens.push_back({ .type = TokenType::double_dot, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '/') {
                consume();
                tokens.push_back({ .type = TokenType::fslash, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '<') {
                consume();
                tokens.push_back({ .type = TokenType::less, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '>') {
                consume();
                tokens.push_back({ .type = TokenType::above, .line = line_count, .col = m_col - 1, .file = file });
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