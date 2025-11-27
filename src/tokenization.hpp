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
    char_type,
    proc_ptr,
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
    expr_stmt,
    popfromstack,
    cast_to,
    ct_eval,
    _namespace,
    double_colon,
    ct_mdefined,
    tilda,
    _typedef,
    _constexpr,
    impl,
    raise,
    _try,
    _catch,
    __drvalue,
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

const __map<TokenType_t, std::string> map_tok2str {
    {TokenType_t::exit, "`exit`"},
    {TokenType_t::int_lit, "`int literal`"},
    {TokenType_t::semi, "`;`"},
    {TokenType_t::open_paren, "`(`"},
    {TokenType_t::close_paren, "`)`"},
    {TokenType_t::ident, "identifier"},
    {TokenType_t::let, "`let`"},
    {TokenType_t::eq, "`=`"},
    {TokenType_t::eqeq, "`==`"},
    {TokenType_t::plus, "`+`"},
    {TokenType_t::star, "`*`"},
    {TokenType_t::minus, "`-`"},
    {TokenType_t::fslash, "`/`"},
    {TokenType_t::open_curly, "`{`"},
    {TokenType_t::close_curly, "`}`"},
    {TokenType_t::if_, "`if`"},
    {TokenType_t::elif, "`elif`"},
    {TokenType_t::else_, "`else`"},
    {TokenType_t::proc, "`proc`"},
    {TokenType_t::in, "`in`"},
    {TokenType_t::string_lit, "`string literal`"},
    {TokenType_t::less, "`<`"},
    {TokenType_t::above, "`>`"},
    {TokenType_t::comma, "`,`"},
    {TokenType_t::int_type, "`int`"},
    {TokenType_t::void_type, "`void`"},
    {TokenType_t::double_dot, "`:`"},
    {TokenType_t::wwhile, "`while`"},
    {TokenType_t::_return, "`return`"},
    {TokenType_t::arrow, "`->`"},
    {TokenType_t::ptr_type, "`ptr`"},
    {TokenType_t::char_type, "`char`"},
    {TokenType_t::any_type, "`any`"},
    {TokenType_t::store8, "`store8`"},
    {TokenType_t::store16, "`store16`"},
    {TokenType_t::store32, "`store32`"},
    {TokenType_t::read8, "`rd8`"},
    {TokenType_t::read16, "`rd16`"},
    {TokenType_t::read32, "`rd32`"},
    {TokenType_t::ampersand, "`&`"},
    {TokenType_t::_include, "`include`"},
    {TokenType_t::_not_eq, "`!=`"},
    {TokenType_t::buffer, "`buffer`"},
    {TokenType_t::mod, "`%`"},
    {TokenType_t::_asm, "`asm`"},
    {TokenType_t::cextern, "`cextern`"},
    {TokenType_t::open_bracket, "`[`"},
    {TokenType_t::close_bracket, "`]`"},
    {TokenType_t::_const, "`const`"},
    {TokenType_t::cast, "`cast`"},
    {TokenType_t::_struct, "`struct`"},
    {TokenType_t::_delete, "`delete`"},
    {TokenType_t::dot, "`.`"},
    {TokenType_t::_break, "`break`"},
    {TokenType_t::plus_eq, "`+=`"},
    {TokenType_t::minus_eq, "`-=`"},
    {TokenType_t::star_eq, "`*=`"},
    {TokenType_t::fslash_eq, "`/=`"},
    {TokenType_t::_static_assert, "`static_assert`"},
    {TokenType_t::double_ampersand, "`&&`"},
    {TokenType_t::double_stick, "`||`"},
    {TokenType_t::hash_sign, "`#`"},
    {TokenType_t::_define, "`define`"},
    {TokenType_t::dollar, "`$`"},
    {TokenType_t::_interface, "`interface`"},
    {TokenType_t::_sizeof, "`sizeof`"},
    {TokenType_t::_typeid, "`typeid`"},
    {TokenType_t::oninit, "`oninit`"},
    {TokenType_t::shift_left, "`<<`"},
    {TokenType_t::shift_right, "`>>`"},
    {TokenType_t::pushonstack, "`pushonstack`"},
    {TokenType_t::_line, "`__LINE__`"},
    {TokenType_t::_col, "`__COL__`"},
    {TokenType_t::_file, "`__FILE__`"},
    {TokenType_t::empty_stmt, "`__empty_stmt`"},
    {TokenType_t::_type, "`ct_type`"},
    {TokenType_t::expr_stmt, "`__expr_stmt`"},
    {TokenType_t::popfromstack, "`__popfromstack`"},
    {TokenType_t::cast_to, "`cast_to`"},
    {TokenType_t::ct_eval, "`ct_eval`"},
    {TokenType_t::_namespace, "`namespace`"},
    {TokenType_t::double_colon, "`::`"},
    {TokenType_t::ct_mdefined, "`ct_mdefined`"},
    {TokenType_t::tilda, "`~`"},
    {TokenType_t::_typedef, "`typedef`"},
    {TokenType_t::_constexpr, "`constexpr`"},
    {TokenType_t::proc_ptr, "`ProcPtr`"},
    {TokenType_t::impl, "`impl`"},
    {TokenType_t::raise, "`raise`"},
    {TokenType_t::_try, "`try`"},
    {TokenType_t::_catch, "`catch`"},
    {TokenType_t::__drvalue, "`__disable_rvalue__`"}
};

std::string tok_to_string(const TokenType_t type)
{
    const auto& search = map_tok2str.find(type);
    assert(search != map_tok2str.end());
    return search->second;
}

inline std::optional<int> bin_prec(const TokenType_t type)
{
    int prec_IOTA = 0;
    switch (type) {
    case TokenType_t::double_ampersand:
    case TokenType_t::double_stick:
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
    case TokenType_t::dot:
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
    std::optional<Token*> expand;
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

void putloc(const Token& tok) {
    printf("%s %d:%d", tok.file.c_str(), tok.line, tok.col);
}

std::string loc_of(const Token& tok) {
    static char buffer[2048];
    sprintf(buffer, "%s %d:%d", tok.file.c_str(), tok.line, tok.col);
    std::string str(buffer);
    return str;
}

bool is_valid_id(const char c) {
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

    TokenizerResult tokenize(const std::string& file)
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
                    tokens->push_back({ .type = TokenType_t::exit, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "let") {
                    tokens->push_back({ .type = TokenType_t::let, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "if") {
                    tokens->push_back({ .type = TokenType_t::if_, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "elif") {
                    tokens->push_back({ .type = TokenType_t::elif, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "else") {
                    tokens->push_back({ .type = TokenType_t::else_, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "proc") {
                    tokens->push_back({ .type = TokenType_t::proc, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "int") {
                    tokens->push_back({ .type = TokenType_t::int_type, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "void") {
                    tokens->push_back({ .type = TokenType_t::void_type, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "ptr") {
                    tokens->push_back({ .type = TokenType_t::ptr_type, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "char") {
                    tokens->push_back({ .type = TokenType_t::char_type, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "any") {
                    tokens->push_back({ .type = TokenType_t::any_type, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "while") {
                    tokens->push_back({ .type = TokenType_t::wwhile, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "return") {
                    tokens->push_back({ .type = TokenType_t::_return, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "store8") {
                    tokens->push_back({ .type = TokenType_t::store8, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "store16") {
                    tokens->push_back({ .type = TokenType_t::store16, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "store32") {
                    tokens->push_back({ .type = TokenType_t::store32, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "rd8") {
                    tokens->push_back({ .type = TokenType_t::read8, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "rd16") {
                    tokens->push_back({ .type = TokenType_t::read16, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "rd32") {
                    tokens->push_back({ .type = TokenType_t::read32, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "include") {
                    tokens->push_back({ .type = TokenType_t::_include, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "buffer") {
                    tokens->push_back({ .type = TokenType_t::buffer, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "asm") {
                    tokens->push_back({ .type = TokenType_t::_asm, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "cextern") {
                    tokens->push_back({ .type = TokenType_t::cextern, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "const") {
                    tokens->push_back({ .type = TokenType_t::_const, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "cast") {
                    tokens->push_back({ .type = TokenType_t::cast, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "struct") {
                    tokens->push_back({ .type = TokenType_t::_struct, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "delete") {
                    tokens->push_back({ .type = TokenType_t::_delete, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "break") {
                    tokens->push_back({ .type = TokenType_t::_break, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "static_assert") {
                    tokens->push_back({ .type = TokenType_t::_static_assert, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "define") {
                    tokens->push_back({ .type = TokenType_t::_define, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "interface") {
                    tokens->push_back({ .type = TokenType_t::_interface, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "sizeof") {
                    tokens->push_back({ .type = TokenType_t::_sizeof, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "typeid") {
                    tokens->push_back({ .type = TokenType_t::_typeid, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "__oninit") {
                    tokens->push_back({ .type = TokenType_t::oninit, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "__pushonstack") {
                    tokens->push_back({ .type = TokenType_t::pushonstack, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "__popfromstack") {
                    tokens->push_back({ .type = TokenType_t::popfromstack, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "__LINE__") {
                    tokens->push_back({ .type = TokenType_t::_line, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "__COL__") {
                    tokens->push_back({ .type = TokenType_t::_col, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "__FILE__") {
                    tokens->push_back({ .type = TokenType_t::_file, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "__empty_stmt") {
                    tokens->push_back({ .type = TokenType_t::empty_stmt, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "ct_type") {
                    tokens->push_back({ .type = TokenType_t::_type, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "__expr_stmt") {
                    tokens->push_back({ .type = TokenType_t::expr_stmt, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "cast_to") {
                    tokens->push_back({ .type = TokenType_t::cast_to, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "ct_eval") {
                    tokens->push_back({ .type = TokenType_t::ct_eval, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "namespace") {
                    tokens->push_back({ .type = TokenType_t::_namespace, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "ct_mdefined") {
                    tokens->push_back({ .type = TokenType_t::ct_mdefined, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "typedef") {
                    tokens->push_back({ .type = TokenType_t::_typedef, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "constexpr") {
                    tokens->push_back({ .type = TokenType_t::_constexpr, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "ProcPtr") {
                    tokens->push_back({ .type = TokenType_t::proc_ptr, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "impl") {
                    tokens->push_back({ .type = TokenType_t::impl, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "raise") {
                    tokens->push_back({ .type = TokenType_t::raise, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "try") {
                    tokens->push_back({ .type = TokenType_t::_try, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "catch") {
                    tokens->push_back({ .type = TokenType_t::_catch, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else if (buf == "__disable_rvalue__") {
                    tokens->push_back({ .type = TokenType_t::__drvalue, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .file = file, .expand = std::nullopt });
                    buf.clear();
                }
                else {
                    tokens->push_back({ .type = TokenType_t::ident, .line =  line_count, .col =  m_col - static_cast<int>(buf.size()), .value = buf, .file = file, .expand = std::nullopt });
                    buf.clear();
                }
            }
            else if (std::isdigit(peek().value())) {
                buf.push_back(consume());
                while (peek().has_value() && std::isdigit(peek().value())) {
                    buf.push_back(consume());
                }
                tokens->push_back({ .type = TokenType_t::int_lit, .line = line_count, .col = m_col - static_cast<int>(buf.size()), .value = buf, .file = file, .expand = std::nullopt });
                buf.clear();
            }
            else if (peek().value() == '-' && peek(1).has_value() && peek(1).value() == '>') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::arrow, .line =  line_count, .col = m_col - 2, .file = file, .expand = std::nullopt });
            }
            else if (peek().value() == '<' && peek(1).has_value() && peek(1).value() == '<') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::shift_left, .line =  line_count, .col = m_col - 2, .file = file, .expand = std::nullopt });
            }
            else if (peek().value() == '>' && peek(1).has_value() && peek(1).value() == '>') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::shift_right, .line =  line_count, .col = m_col - 2, .file = file, .expand = std::nullopt });
            }
            else if (peek().value() == '&' && peek(1).has_value() && peek(1).value() == '&') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::double_ampersand, .line =  line_count, .col = m_col - 2, .file = file, .expand = std::nullopt });
            }
            else if (peek().value() == '|' && peek(1).has_value() && peek(1).value() == '|') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::double_stick, .line =  line_count, .col = m_col - 2, .file = file, .expand = std::nullopt });
            }
            else if (peek().value() == ':' && peek(1).has_value() && peek(1).value() == ':') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::double_colon, .line =  line_count, .col = m_col - 2, .file = file, .expand = std::nullopt });
            }
            else if (peek().value() == '+' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::plus_eq, .line =  line_count, .col = m_col - 2, .file = file, .expand = std::nullopt });
            }
            else if (peek().value() == '-' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::minus_eq, .line =  line_count, .col = m_col - 2, .file = file, .expand = std::nullopt });
            }
            else if (peek().value() == '*' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::star_eq, .line =  line_count, .col = m_col - 2, .file = file, .expand = std::nullopt });
            }
            else if (peek().value() == '/' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::fslash_eq, .line =  line_count, .col = m_col - 2, .file = file, .expand = std::nullopt });
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
                        else if(buf[i+1] == 't') {
                            buf.erase(buf.begin()+i);
                            buf[i] = '\t';
                        }
                        else if(buf[i+1] == '"') {
                            buf.erase(buf.begin()+i);
                            buf[i] = '"';
                        }
                    }
                }
                tokens->push_back({ .type = TokenType_t::string_lit, .line = line_count , .col = m_col - static_cast<int>(buf.size()), .value = buf, .file = file, .expand = std::nullopt });
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
                tokens->push_back({ .type = TokenType_t::int_lit, .line = line_count , .col = m_col - static_cast<int>(buf.size()), .value = std::to_string(static_cast<int>(buf[0])), .file = file, .expand = std::nullopt });
                buf.clear();
            }
            else if (peek().value() == '(') {
                consume();
                tokens->push_back({ .type = TokenType_t::open_paren, .line =  line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == ')') {
                consume();
                tokens->push_back({ .type = TokenType_t::close_paren, .line =  line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '~') {
                consume();
                tokens->push_back({ .type = TokenType_t::tilda, .line =  line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '[') {
                consume();
                tokens->push_back({ .type = TokenType_t::open_bracket, .line =  line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == ']') {
                consume();
                tokens->push_back({ .type = TokenType_t::close_bracket, .line =  line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == ';') {
                consume();
                tokens->push_back({ .type = TokenType_t::semi, .line =  line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '=' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::eqeq, .line = line_count, .col = m_col - 2, .file = file, .expand = {} });
            }
            else if (peek().value() == '!' && peek(1).has_value() && peek(1).value() == '=') {
                consume();
                consume();
                tokens->push_back({ .type = TokenType_t::_not_eq, .line = line_count, .col = m_col - 2, .file = file, .expand = {} });
            }
            else if (peek().value() == '=') {
                consume();
                tokens->push_back({ .type = TokenType_t::eq, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '+') {
                consume();
                tokens->push_back({ .type = TokenType_t::plus, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '*') {
                consume();
                tokens->push_back({ .type = TokenType_t::star, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '-') {
                consume();
                tokens->push_back({ .type = TokenType_t::minus, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '%') {
                consume();
                tokens->push_back({ .type = TokenType_t::mod, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == ',') {
                consume();
                tokens->push_back({ .type = TokenType_t::comma, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '.') {
                consume();
                tokens->push_back({ .type = TokenType_t::dot, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '&') {
                consume();
                tokens->push_back({ .type = TokenType_t::ampersand, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '#') {
                consume();
                tokens->push_back({ .type = TokenType_t::hash_sign, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '$') {
                consume();
                tokens->push_back({ .type = TokenType_t::dollar, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == ':') {
                consume();
                tokens->push_back({ .type = TokenType_t::double_dot, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '/') {
                consume();
                tokens->push_back({ .type = TokenType_t::fslash, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '<') {
                consume();
                tokens->push_back({ .type = TokenType_t::less, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '>') {
                consume();
                tokens->push_back({ .type = TokenType_t::above, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '{') {
                consume();
                tokens->push_back({ .type = TokenType_t::open_curly, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
            }
            else if (peek().value() == '}') {
                consume();
                tokens->push_back({ .type = TokenType_t::close_curly, .line = line_count, .col = m_col - 1, .file = file, .expand = {} });
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