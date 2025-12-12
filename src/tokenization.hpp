#pragma once

enum class TokenType_t {
    exit, int_lit, semi, open_paren, close_paren, ident, let, _type, less, eqeq, eq,
    plus, star, minus, fslash, above, open_curly, close_curly, if_, elif, else_,
    proc, in, string_lit, comma, int_type, double_dot, wwhile, _return, arrow,
    void_type, ptr_type, any_type, char_type, proc_ptr, store8, store16, store32,
    read8, read16, read32, ampersand, _include, _not_eq, buffer, mod, _asm,
    cextern, open_bracket, close_bracket, _const, cast, _struct, _delete, dot,
    _break, plus_eq, minus_eq, star_eq, fslash_eq, _static_assert, double_ampersand,
    double_stick, hash_sign, _define, dollar, _interface, _sizeof, _typeid, oninit,
    shift_left, shift_right, pushonstack, _line, _col, _file, empty_stmt, expr_stmt,
    popfromstack, cast_to, ct_eval, _namespace, double_colon, ct_mdefined, tilda,
    _typedef, _constexpr, impl, raise, _try, _catch, __drvalue, for_, foreach_, enum_,
    at_sign,
};

static const char* const token_names[] = {
    "`exit`", "`int literal`", "`;`", "`(`", "`)`", "identifier", "`let`", "`ct_type`",
    "`<`", "`==`", "`=`", "`+`", "`*`", "`-`", "`/`", "`>`", "`{`", "`}`",
    "`if`", "`elif`", "`else`", "`proc`", "`in`", "`string literal`", "`,`",
    "`int`", "`:`", "`while`", "`return`", "`->`", "`void`", "`ptr`", "`any`", "`char`",
    "`ProcPtr`", "`store8`", "`store16`", "`store32`", "`rd8`", "`rd16`", "`rd32`",
    "`&`", "`include`", "`!=`", "`buffer`", "`%`", "`asm`", "`cextern`", "`[`", "`]`",
    "`const`", "`cast`", "`struct`", "`delete`", "`.`", "`break`", "`+=`", "`-=`",
    "`*=`", "`/=`", "`static_assert`", "`&&`", "`||`", "`#`", "`define`", "`$`",
    "`interface`", "`sizeof`", "`typeid`", "`oninit`", "`<<`", "`>>`", "`pushonstack`",
    "`__LINE__`", "`__COL__`", "`__FILE__`", "`__empty_stmt`", "`__expr_stmt`",
    "`__popfromstack`", "`cast_to`", "`ct_eval`", "`namespace`", "`::`", "`ct_mdefined`",
    "`~`", "`typedef`", "`constexpr`", "`impl`", "`raise`", "`try`", "`catch`",
    "`__disable_rvalue__`", "`for`", "`foreach`", "`enum`", "`@`",
};

inline GString tok_to_string(const TokenType_t type) {
    return token_names[static_cast<size_t>(type)];
}

inline std::optional<int> bin_prec(const TokenType_t type) {
    switch (type) {
        case TokenType_t::double_stick: return 0;
        case TokenType_t::double_ampersand: return 1;
        case TokenType_t::eqeq: case TokenType_t::_not_eq: return 2;
        case TokenType_t::less: case TokenType_t::above: return 3;
        case TokenType_t::shift_left: case TokenType_t::shift_right: return 4;
        case TokenType_t::plus: case TokenType_t::minus: return 5;
        case TokenType_t::star: case TokenType_t::fslash: case TokenType_t::mod: return 6;
        case TokenType_t::open_bracket: return 100;
        case TokenType_t::dot: return 10000;
        default: return {};
    }
}

struct Token {
    TokenType_t type;
    int line;
    int col;
    std::optional<GString> value {};
    GString file;
    std::optional<Token*> expand;

    friend std::ostream& operator<<(std::ostream& out, const Token& tok) {
        out << "Token(.type = " << tok_to_string(tok.type)
            << ", .line = " << tok.line << ", .col = " << tok.col;
        if (tok.value.has_value()) out << ", .value = " << tok.value.value();
        out << ")";
        return out;
    }
};

void putloc(const Token& tok) {
    printf("%s %d:%d", tok.file.c_str(), tok.line, tok.col);
}

GString loc_of(const Token& tok) {
    char buffer[512];
    std::snprintf(buffer, sizeof(buffer), "%s %d:%d", tok.file.c_str(), tok.line, tok.col);
    return GString(buffer);
}

struct TokenizerResult {
    GVector<Token>* tokens;
    GVector<GString>* lines;
};

static constexpr std::array<uint8_t, 256> char_map = []() {
    std::array<uint8_t, 256> t = {};
    t[' '] = t['\t'] = t['\r'] = t['\v'] = t['\f'] = 1;
    t['\n'] = 2;
    for (int i = '0'; i <= '9'; ++i) t[i] = 3;
    for (int i = 'a'; i <= 'z'; ++i) t[i] = 4;
    for (int i = 'A'; i <= 'Z'; ++i) t[i] = 4;
    t['_'] = 4;
    return t;
}();

class Tokenizer {
public:
    explicit Tokenizer(GString src) : m_src(std::move(src)) {
        if (keywords.empty()) init_keywords();
        lines = split_string(m_src, "\n");
    }

    TokenizerResult tokenize(const GString& file) {
        GVector<Token>* tokens = g_GlobalArena->emplace<GVector<Token>>();
        tokens->reserve(m_src.size() / 4);

        const char* curr = m_src.c_str();
        const char* const end = curr + m_src.size();
        const char* line_start = curr;
        int line = 1;

        while (curr < end) {
            const uint8_t type = char_map[static_cast<uint8_t>(*curr)];

            if (type == 1) { 
                ++curr; 
                continue; 
            }
            
            if (type == 2) { 
                ++line;
                ++curr;
                line_start = curr;
                continue;
            }

            const int col = static_cast<int>(curr - line_start) + 1;

            if (type == 4) {
                const char* start = curr;
                do { ++curr; } while (curr < end && char_map[static_cast<uint8_t>(*curr)] >= 3);
                
                GString text(start, curr - start);
                auto it = keywords.find(text);
                if (it != keywords.end()) {
                    tokens->push_back({it->second, line, col, {}, file, {}});
                } else {
                    tokens->push_back({TokenType_t::ident, line, col, std::move(text), file, {}});
                }
                continue;
            }

            if (type == 3) {
                const char* start = curr;
                do { ++curr; } while (curr < end && char_map[static_cast<uint8_t>(*curr)] == 3);
                tokens->push_back({TokenType_t::int_lit, line, col, GString(start, curr - start), file, {}});
                continue;
            }

            const char c = *curr;
            switch (c) {
                case '(': tokens->push_back({TokenType_t::open_paren, line, col, {}, file, {}}); ++curr; break;
                case ')': tokens->push_back({TokenType_t::close_paren, line, col, {}, file, {}}); ++curr; break;
                case '{': tokens->push_back({TokenType_t::open_curly, line, col, {}, file, {}}); ++curr; break;
                case '}': tokens->push_back({TokenType_t::close_curly, line, col, {}, file, {}}); ++curr; break;
                case '[': tokens->push_back({TokenType_t::open_bracket, line, col, {}, file, {}}); ++curr; break;
                case ']': tokens->push_back({TokenType_t::close_bracket, line, col, {}, file, {}}); ++curr; break;
                case ';': tokens->push_back({TokenType_t::semi, line, col, {}, file, {}}); ++curr; break;
                case ',': tokens->push_back({TokenType_t::comma, line, col, {}, file, {}}); ++curr; break;
                case '.': tokens->push_back({TokenType_t::dot, line, col, {}, file, {}}); ++curr; break;
                case '~': tokens->push_back({TokenType_t::tilda, line, col, {}, file, {}}); ++curr; break;
                case '$': tokens->push_back({TokenType_t::dollar, line, col, {}, file, {}}); ++curr; break;
                case '#': tokens->push_back({TokenType_t::hash_sign, line, col, {}, file, {}}); ++curr; break;
                case '%': tokens->push_back({TokenType_t::mod, line, col, {}, file, {}}); ++curr; break;
                case '@': tokens->push_back({TokenType_t::at_sign, line, col, {}, file, {}}); ++curr; break;

                case '=':
                    if (curr + 1 < end && curr[1] == '=') {
                        tokens->push_back({TokenType_t::eqeq, line, col, {}, file, {}}); curr += 2;
                    } else {
                        tokens->push_back({TokenType_t::eq, line, col, {}, file, {}}); ++curr;
                    }
                    break;
                case '+':
                    if (curr + 1 < end && curr[1] == '=') {
                        tokens->push_back({TokenType_t::plus_eq, line, col, {}, file, {}}); curr += 2;
                    } else {
                        tokens->push_back({TokenType_t::plus, line, col, {}, file, {}}); ++curr;
                    }
                    break;
                case '-':
                    if (curr + 1 < end) {
                        const char n = curr[1];
                        if (n == '>') { tokens->push_back({TokenType_t::arrow, line, col, {}, file, {}}); curr += 2; }
                        else if (n == '=') { tokens->push_back({TokenType_t::minus_eq, line, col, {}, file, {}}); curr += 2; }
                        else { tokens->push_back({TokenType_t::minus, line, col, {}, file, {}}); ++curr; }
                    } else {
                        tokens->push_back({TokenType_t::minus, line, col, {}, file, {}}); ++curr;
                    }
                    break;
                case '*':
                    if (curr + 1 < end && curr[1] == '=') {
                        tokens->push_back({TokenType_t::star_eq, line, col, {}, file, {}}); curr += 2;
                    } else {
                        tokens->push_back({TokenType_t::star, line, col, {}, file, {}}); ++curr;
                    }
                    break;
                case '!':
                    if (curr + 1 < end && curr[1] == '=') {
                        tokens->push_back({TokenType_t::_not_eq, line, col, {}, file, {}}); curr += 2;
                    } else {
                         std::cerr << "Unexpected token '!' at " << file.c_str() << " " << line << ":" << col << std::endl; exit(1);
                    }
                    break;
                case '<':
                    if (curr + 1 < end && curr[1] == '<') {
                        tokens->push_back({TokenType_t::shift_left, line, col, {}, file, {}}); curr += 2;
                    } else {
                        tokens->push_back({TokenType_t::less, line, col, {}, file, {}}); ++curr;
                    }
                    break;
                case '>':
                    if (curr + 1 < end && curr[1] == '>') {
                        tokens->push_back({TokenType_t::shift_right, line, col, {}, file, {}}); curr += 2;
                    } else {
                        tokens->push_back({TokenType_t::above, line, col, {}, file, {}}); ++curr;
                    }
                    break;
                case '&':
                    if (curr + 1 < end && curr[1] == '&') {
                        tokens->push_back({TokenType_t::double_ampersand, line, col, {}, file, {}}); curr += 2;
                    } else {
                        tokens->push_back({TokenType_t::ampersand, line, col, {}, file, {}}); ++curr;
                    }
                    break;
                case '|':
                    if (curr + 1 < end && curr[1] == '|') {
                        tokens->push_back({TokenType_t::double_stick, line, col, {}, file, {}}); curr += 2;
                    } else {
                         std::cerr << "Unexpected token '|' at " << file.c_str() << " " << line << ":" << col << std::endl; exit(1);
                    }
                    break;
                case ':':
                    if (curr + 1 < end && curr[1] == ':') {
                        tokens->push_back({TokenType_t::double_colon, line, col, {}, file, {}}); curr += 2;
                    } else {
                        tokens->push_back({TokenType_t::double_dot, line, col, {}, file, {}}); ++curr;
                    }
                    break;
                case '/':
                    if (curr + 1 < end) {
                        const char n = curr[1];
                        if (n == '/') {
                            curr += 2;
                            while (curr < end && *curr != '\n') ++curr;
                        } else if (n == '*') {
                            curr += 2;
                            while (curr + 1 < end) {
                                if (*curr == '*' && curr[1] == '/') { curr += 2; break; }
                                if (*curr == '\n') { ++line; line_start = curr + 1; }
                                ++curr;
                            }
                        } else if (n == '=') {
                            tokens->push_back({TokenType_t::fslash_eq, line, col, {}, file, {}}); curr += 2;
                        } else {
                            tokens->push_back({TokenType_t::fslash, line, col, {}, file, {}}); ++curr;
                        }
                    } else {
                        tokens->push_back({TokenType_t::fslash, line, col, {}, file, {}}); ++curr;
                    }
                    break;
                case '"': {
                    ++curr;
                    const char* start = curr;
                    GString buf;
                    while (curr < end) {
                        if (*curr == '"') break;
                        if (*curr == '\\') {
                            buf.append(start, curr - start);
                            ++curr;
                            if (curr >= end) break;
                            switch (*curr) {
                                case 'n': buf += '\n'; break;
                                case 't': buf += '\t'; break;
                                case '"': buf += '"'; break;
                                case '\\': buf += '\\'; break;
                                default: buf += *curr; break;
                            }
                            ++curr;
                            start = curr;
                            continue;
                        }
                        if (*curr == '\n') { ++line; line_start = curr + 1; }
                        ++curr;
                    }
                    if (buf.empty()) {
                        tokens->push_back({TokenType_t::string_lit, line, col, GString(start, curr - start), file, {}});
                    } else {
                        buf.append(start, curr - start);
                        tokens->push_back({TokenType_t::string_lit, line, col, std::move(buf), file, {}});
                    }
                    if (curr < end) ++curr;
                    break;
                }
                case '\'': {
                    ++curr;
                    GString buf;
                    if (curr < end && *curr == '\\') {
                        ++curr;
                        if (curr < end) {
                            char v = *curr;
                            if (v == 'n') v = '\n'; else if (v == 't') v = '\t'; else if (v == '"') v = '"'; else if (v == '\'') v = '\'';
                            buf = std::to_string(static_cast<int>(v));
                            ++curr;
                        }
                    } else if (curr < end) {
                        buf = std::to_string(static_cast<int>(*curr));
                        ++curr;
                    }
                    if (curr < end && *curr == '\'') ++curr;
                    tokens->push_back({TokenType_t::int_lit, line, col, std::move(buf), file, {}});
                    break;
                }
                default:
                    std::cerr << "Invalid token '" << c << "' at " << file.c_str() << " " << line << ":" << col << std::endl;
                    exit(EXIT_FAILURE);
            }
        }
        return {.tokens = tokens, .lines = lines};
    }

    GVector<GString>* lines;

private:
    const GString m_src;
    static GMap<GString, TokenType_t> keywords;

    static void init_keywords() {
        keywords.reserve(128);
        keywords = {
            {"exit", TokenType_t::exit}, {"let", TokenType_t::let}, {"if", TokenType_t::if_},
            {"elif", TokenType_t::elif}, {"else", TokenType_t::else_}, {"proc", TokenType_t::proc},
            {"int", TokenType_t::int_type}, {"void", TokenType_t::void_type}, {"ptr", TokenType_t::ptr_type},
            {"char", TokenType_t::char_type}, {"any", TokenType_t::any_type}, {"while", TokenType_t::wwhile},
            {"return", TokenType_t::_return}, {"store8", TokenType_t::store8}, {"store16", TokenType_t::store16},
            {"store32", TokenType_t::store32}, {"rd8", TokenType_t::read8}, {"rd16", TokenType_t::read16},
            {"rd32", TokenType_t::read32}, {"include", TokenType_t::_include}, {"buffer", TokenType_t::buffer},
            {"asm", TokenType_t::_asm}, {"cextern", TokenType_t::cextern}, {"const", TokenType_t::_const},
            {"cast", TokenType_t::cast}, {"struct", TokenType_t::_struct}, {"delete", TokenType_t::_delete},
            {"break", TokenType_t::_break}, {"static_assert", TokenType_t::_static_assert},
            {"define", TokenType_t::_define}, {"interface", TokenType_t::_interface},
            {"sizeof", TokenType_t::_sizeof}, {"typeid", TokenType_t::_typeid}, {"__oninit", TokenType_t::oninit},
            {"__pushonstack", TokenType_t::pushonstack}, {"__popfromstack", TokenType_t::popfromstack},
            {"__LINE__", TokenType_t::_line}, {"__COL__", TokenType_t::_col}, {"__FILE__", TokenType_t::_file},
            {"__empty_stmt", TokenType_t::empty_stmt}, {"ct_type", TokenType_t::_type},
            {"__expr_stmt", TokenType_t::expr_stmt}, {"cast_to", TokenType_t::cast_to},
            {"ct_eval", TokenType_t::ct_eval}, {"namespace", TokenType_t::_namespace},
            {"ct_mdefined", TokenType_t::ct_mdefined}, {"typedef", TokenType_t::_typedef},
            {"constexpr", TokenType_t::_constexpr}, {"ProcPtr", TokenType_t::proc_ptr},
            {"impl", TokenType_t::impl}, {"raise", TokenType_t::raise}, {"try", TokenType_t::_try},
            {"catch", TokenType_t::_catch}, {"__disable_rvalue__", TokenType_t::__drvalue},
            {"foreach", TokenType_t::foreach_}, {"for", TokenType_t::for_}, {"enum", TokenType_t::enum_}
        };
    }
};

GMap<GString, TokenType_t> Tokenizer::keywords;