#pragma once

enum class TokenType_t {
    exit,
    int_lit,
    semi,
    open_paren,
    close_paren,
    ident,
    let,
    _type,
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
    proc,
    in,
    string_lit,
    comma,
    int_type,
    double_dot,
    wwhile,
    _return,
    arrow,
    void_type,
    ptr_type,
    any_type,
    store8,
    store16,
    store32,
    read8,
    read16,
    read32,
    ampersand,
    _include,
    _not_eq,
    buffer,
    mod,
    _asm,
    cextern,
    open_bracket,
    close_bracket,
    _const,
    cast,
    _struct,
    _delete,
    dot,
    _break,
    plus_eq,
    minus_eq,
    star_eq,
    fslash_eq,
    _static_assert,
    double_ampersand,
    double_stick,
    hash_sign,
    _define,
    dollar,
    _interface,
    _sizeof,
    _typeid,
    oninit,
    shift_left,
    shift_right,
    pushonstack,
    _line,
    _col,
    _file,
    empty_stmt,
};

std::vector<std::string>* split_string(const std::string& str, const std::string& delimiter) {
    std::vector<std::string>* strings = new std::vector<std::string>;
    std::string::size_type pos = 0;
    std::string::size_type prev = 0;
    while((pos = str.find(delimiter, prev)) != std::string::npos) {
        strings->push_back(str.substr(prev, pos - prev));
        prev = pos + delimiter.size();
    }
    strings->push_back(str.substr(prev));
    return strings;
}

std::string tok_to_string(const TokenType_t type)
{
    switch (type) {
    case TokenType_t::exit:
        return "`exit`";
    case TokenType_t::int_lit:
        return "`int literal`";
    case TokenType_t::semi:
        return "`;`";
    case TokenType_t::open_paren:
        return "`(`";
    case TokenType_t::close_paren:
        return "`)`";
    case TokenType_t::ident:
        return "identifier";
    case TokenType_t::let:
        return "`let`";
    case TokenType_t::eq:
        return "`=`";
    case TokenType_t::eqeq:
        return "`==`";
    case TokenType_t::plus:
        return "`+`";
    case TokenType_t::star:
        return "`*`";
    case TokenType_t::minus:
        return "`-`";
    case TokenType_t::fslash:
        return "`/`";
    case TokenType_t::open_curly:
        return "`{`";
    case TokenType_t::close_curly:
        return "`}`";
    case TokenType_t::if_:
        return "`if`";
    case TokenType_t::elif:
        return "`elif`";
    case TokenType_t::else_:
        return "`else`";
    case TokenType_t::proc:
        return "`proc`";
    case TokenType_t::in:
        return "`in`";
    case TokenType_t::string_lit:
        return "`string literal`";
    case TokenType_t::less:
        return "`<`";
    case TokenType_t::above:
        return "`>`";
    case TokenType_t::comma:
        return "`,`";
    case TokenType_t::int_type:
        return "`int`";
    case TokenType_t::void_type:
        return "`void`";
    case TokenType_t::double_dot:
        return "`:`";
    case TokenType_t::wwhile:
        return "`while`";
    case TokenType_t::_return:
        return "`return`";
    case TokenType_t::arrow:
        return "`->`";
    case TokenType_t::ptr_type:
        return "`ptr`";
    case TokenType_t::any_type:
        return "`any`";
    case TokenType_t::store8:
        return "`store8`";
    case TokenType_t::store16:
        return "`store16`";
    case TokenType_t::store32:
        return "`store32`";
    case TokenType_t::read8:
        return "`rd8`";
    case TokenType_t::read16:
        return "`rd16`";
    case TokenType_t::read32:
        return "`rd32`";
    case TokenType_t::ampersand:
        return "`&`";
    case TokenType_t::_include:
        return "`include`";
    case TokenType_t::_not_eq:
        return "`!=`";
    case TokenType_t::buffer:
        return "`buffer`";
    case TokenType_t::mod:
        return "`%`";
    case TokenType_t::_asm:
        return "`asm`";
    case TokenType_t::cextern:
        return "`cextern`";
    case TokenType_t::open_bracket:
        return "`[`";
    case TokenType_t::close_bracket:
        return "`]`";
    case TokenType_t::_const:
        return "`const`";
    case TokenType_t::cast:
        return "`cast`";
    case TokenType_t::_struct:
        return "`struct`";
    case TokenType_t::_delete:
        return "`delete`";
    case TokenType_t::dot:
        return "`.`";
    case TokenType_t::_break:
        return "`break`";
    case TokenType_t::plus_eq:
        return "`+=`";
    case TokenType_t::minus_eq:
        return "`-=`";
    case TokenType_t::star_eq:
        return "`*=`";
    case TokenType_t::fslash_eq:
        return "`/=`";
    case TokenType_t::_static_assert:
        return "`static_assert`";
    case TokenType_t::double_ampersand:
        return "`&&`";
    case TokenType_t::double_stick:
        return "`||`";
    case TokenType_t::hash_sign:
        return "`#`";
    case TokenType_t::_define:
        return "`define`";
    case TokenType_t::dollar:
        return "`$`";
    case TokenType_t::_interface:
        return "`interface`";
    case TokenType_t::_sizeof:
        return "`sizeof`";
    case TokenType_t::_typeid:
        return "`typeid`";
    case TokenType_t::oninit:
        return "`oninit`";
    case TokenType_t::shift_left:
        return "`<<`";
    case TokenType_t::shift_right:
        return "`>>`";
    case TokenType_t::pushonstack:
        return "`pushonstack`";
    case TokenType_t::_line:
        return "`__LINE__`";
    case TokenType_t::_col:
        return "`__COL__`";
    case TokenType_t::_file:
        return "`__FILE__`";
    case TokenType_t::empty_stmt:
        return "`__empty_stmt`";
    case TokenType_t::_type:
        return "`ct_type`";
    }
    assert(false);
}

int prec_IOTA = 0;
inline std::optional<int> bin_prec(const TokenType_t type)
{
    switch (type) {
    case TokenType_t::double_ampersand:
    case TokenType_t::double_stick:
        return prec_IOTA++;
    case TokenType_t::dot:
        return prec_IOTA++;
    case TokenType_t::eqeq:
    case TokenType_t::_not_eq:
    case TokenType_t::less:
    case TokenType_t::above:
        return prec_IOTA++;
    case TokenType_t::minus:
    case TokenType_t::plus:
    case TokenType_t::shift_left:
    case TokenType_t::shift_right:
        return prec_IOTA++;
    case TokenType_t::fslash:
    case TokenType_t::star:
    case TokenType_t::mod:
        return prec_IOTA++;
    default:
        return {};
    }
    return {};
}

struct Token {
    TokenType_t type;
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

struct TokenizerResult {
    std::vector<Token>* tokens;
    std::vector<std::string>* lines;
};

class Tokenizer {
public:
    explicit Tokenizer(std::string src)
        : m_src(std::move(src)) ,
          m_allocator(1024)
    {
        lines = split_string(m_src, "\n");
    }

    TokenizerResult tokenize(std::string file)
    {
        std::vector<Token>* tokens = new std::vector<Token>;
        std::string buf;
        int line_count = 1;
        while (peek().has_value()) {
            if (std::isalpha(peek().value()) || is_valid_id(peek().value())) {
                buf.push_back(consume());
                while (peek().has_value() && (std::isalnum(peek().value()) || is_valid_id(peek().value()))) {
                    buf.push_back(consume());
                }
                if (buf == "exit") {
                    tokens->push_back({ .type = TokenType_t::exit, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "let") {
                    tokens->push_back({ .type = TokenType_t::let, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "if") {
                    tokens->push_back({ .type = TokenType_t::if_, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "elif") {
                    tokens->push_back({ .type = TokenType_t::elif, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "else") {
                    tokens->push_back({ .type = TokenType_t::else_, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "proc") {
                    tokens->push_back({ .type = TokenType_t::proc, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "int") {
                    tokens->push_back({ .type = TokenType_t::int_type, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "void") {
                    tokens->push_back({ .type = TokenType_t::void_type, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "ptr") {
                    tokens->push_back({ .type = TokenType_t::ptr_type, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "any") {
                    tokens->push_back({ .type = TokenType_t::any_type, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "while") {
                    tokens->push_back({ .type = TokenType_t::wwhile, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "return") {
                    tokens->push_back({ .type = TokenType_t::_return, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "store8") {
                    tokens->push_back({ .type = TokenType_t::store8, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "store16") {
                    tokens->push_back({ .type = TokenType_t::store16, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "store32") {
                    tokens->push_back({ .type = TokenType_t::store32, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "rd8") {
                    tokens->push_back({ .type = TokenType_t::read8, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "rd16") {
                    tokens->push_back({ .type = TokenType_t::read16, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "rd32") {
                    tokens->push_back({ .type = TokenType_t::read32, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "include") {
                    tokens->push_back({ .type = TokenType_t::_include, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "buffer") {
                    tokens->push_back({ .type = TokenType_t::buffer, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "asm") {
                    tokens->push_back({ .type = TokenType_t::_asm, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "cextern") {
                    tokens->push_back({ .type = TokenType_t::cextern, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "const") {
                    tokens->push_back({ .type = TokenType_t::_const, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "cast") {
                    tokens->push_back({ .type = TokenType_t::cast, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "struct") {
                    tokens->push_back({ .type = TokenType_t::_struct, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "delete") {
                    tokens->push_back({ .type = TokenType_t::_delete, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "break") {
                    tokens->push_back({ .type = TokenType_t::_break, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "static_assert") {
                    tokens->push_back({ .type = TokenType_t::_static_assert, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "define") {
                    tokens->push_back({ .type = TokenType_t::_define, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "interface") {
                    tokens->push_back({ .type = TokenType_t::_interface, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "sizeof") {
                    tokens->push_back({ .type = TokenType_t::_sizeof, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "typeid") {
                    tokens->push_back({ .type = TokenType_t::_typeid, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "__oninit") {
                    tokens->push_back({ .type = TokenType_t::oninit, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "__pushonstack") {
                    tokens->push_back({ .type = TokenType_t::pushonstack, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "__LINE__") {
                    tokens->push_back({ .type = TokenType_t::_line, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "__COL__") {
                    tokens->push_back({ .type = TokenType_t::_col, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "__FILE__") {
                    tokens->push_back({ .type = TokenType_t::_file, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "__empty_stmt") {
                    tokens->push_back({ .type = TokenType_t::empty_stmt, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else if (buf == "ct_type") {
                    tokens->push_back({ .type = TokenType_t::_type, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file });
                    buf.clear();
                }
                else {
                    tokens->push_back({ .type = TokenType_t::ident, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .value = buf, .file = file });
                    buf.clear();
                }
            }
            else if (std::isdigit(peek().value())) {
                buf.push_back(consume());
                while (peek().has_value() && std::isdigit(peek().value())) {
                    buf.push_back(consume());
                }
                tokens->push_back({ .type = TokenType_t::int_lit, .line = line_count, .col = m_col - static_cast<int>(buf.size()), .value = buf, .file = file });
                buf.clear();
            }
            else if (peek().value() == '-' && peek(1).has_value() && peek(1).value() == '>') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::arrow, .line =  line_count, .col = m_col - 2, .file = file });
            }
            else if (peek().value() == '<' && peek(1).has_value() && peek(1).value() == '<') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::shift_left, .line =  line_count, .col = m_col - 2, .file = file });
            }
            else if (peek().value() == '>' && peek(1).has_value() && peek(1).value() == '>') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::shift_right, .line =  line_count, .col = m_col - 2, .file = file });
            }
            else if (peek().value() == '&' && peek(1).has_value() && peek(1).value() == '&') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::double_ampersand, .line =  line_count, .col = m_col - 2, .file = file });
            }
            else if (peek().value() == '|' && peek(1).has_value() && peek(1).value() == '|') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::double_stick, .line =  line_count, .col = m_col - 2, .file = file });
            }
            else if (peek().value() == '+' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::plus_eq, .line =  line_count, .col = m_col - 2, .file = file });
            }
            else if (peek().value() == '-' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::minus_eq, .line =  line_count, .col = m_col - 2, .file = file });
            }
            else if (peek().value() == '*' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::star_eq, .line =  line_count, .col = m_col - 2, .file = file });
            }
            else if (peek().value() == '/' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::fslash_eq, .line =  line_count, .col = m_col - 2, .file = file });
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
                    if(peek().has_value() && peek(0).value() == '\\' &&
                        peek(1).has_value() && peek(1).value() == '"') {
                        consume();
                        consume();
                        buf.push_back('"');
                    }
                }
                consume();
                for(int i = 0;i < static_cast<int>(buf.size());++i) {
                    if(buf[i] == '\\') {
                        if(buf[i+1] == 'n') {
                            buf.erase(buf.begin()+i);
                            buf[i] = '\n';
                        }
                        else if(buf[i+1] == '"') {
                            buf.erase(buf.begin()+i);
                            buf[i] = '"';
                        }
                    }
                }
                tokens->push_back({ .type = TokenType_t::string_lit, .line = line_count , .col = m_col - static_cast<int>(buf.size()), .value = buf, .file = file });
                buf.clear();
            }
            else if(peek().value() == '\'') {
                consume();
                buf.clear();
                while(peek().has_value() && peek().value() != '\'') {
                    buf.push_back(consume());
                }
                consume();
                for(int i = 0;i < static_cast<int>(buf.size());++i) {
                    if(buf[i] == '\\') {
                        if(buf[i+1] == 'n') {
                            buf.erase(buf.begin()+i);
                            buf[i] = '\n';
                        }
                        else if(buf[i+1] == '"') {
                            buf.erase(buf.begin()+i);
                            buf[i] = '"';
                        }
                    }
                }
                tokens->push_back({ .type = TokenType_t::int_lit, .line = line_count , .col = m_col - static_cast<int>(buf.size()), .value = std::to_string(static_cast<int>(buf[0])), .file = file });
                buf.clear();
            }
            else if (peek().value() == '(') {
                consume();
                tokens->push_back({ .type = TokenType_t::open_paren, .line =  line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == ')') {
                consume();
                tokens->push_back({ .type = TokenType_t::close_paren, .line =  line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '[') {
                consume();
                tokens->push_back({ .type = TokenType_t::open_bracket, .line =  line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == ']') {
                consume();
                tokens->push_back({ .type = TokenType_t::close_bracket, .line =  line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == ';') {
                consume();
                tokens->push_back({ .type = TokenType_t::semi, .line =  line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '=' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::eqeq, .line = line_count, .col = m_col - 2, .file = file });
            }
            else if (peek().value() == '!' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::_not_eq, .line = line_count, .col = m_col - 2, .file = file });
            }
            else if (peek().value() == '=') {
                consume();
                tokens->push_back({ .type = TokenType_t::eq, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '+') {
                consume();
                tokens->push_back({ .type = TokenType_t::plus, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '*') {
                consume();
                tokens->push_back({ .type = TokenType_t::star, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '-') {
                consume();
                tokens->push_back({ .type = TokenType_t::minus, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '%') {
                consume();
                tokens->push_back({ .type = TokenType_t::mod, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == ',') {
                consume();
                tokens->push_back({ .type = TokenType_t::comma, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '.') {
                consume();
                tokens->push_back({ .type = TokenType_t::dot, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '&') {
                consume();
                tokens->push_back({ .type = TokenType_t::ampersand, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '#') {
                consume();
                tokens->push_back({ .type = TokenType_t::hash_sign, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '$') {
                consume();
                tokens->push_back({ .type = TokenType_t::dollar, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == ':') {
                consume();
                tokens->push_back({ .type = TokenType_t::double_dot, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '/') {
                consume();
                tokens->push_back({ .type = TokenType_t::fslash, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '<') {
                consume();
                tokens->push_back({ .type = TokenType_t::less, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '>') {
                consume();
                tokens->push_back({ .type = TokenType_t::above, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '{') {
                consume();
                tokens->push_back({ .type = TokenType_t::open_curly, .line = line_count, .col = m_col - 1, .file = file });
            }
            else if (peek().value() == '}') {
                consume();
                tokens->push_back({ .type = TokenType_t::close_curly, .line = line_count, .col = m_col - 1, .file = file });
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
        return {.tokens = tokens, .lines = lines};
    }

    std::vector<std::string>* lines;

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
    ArenaAllocator m_allocator;
    size_t m_index = 0;
    int m_col = 1;
};