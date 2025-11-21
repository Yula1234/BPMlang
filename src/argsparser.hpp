#pragma once

enum class FlagType {
    output,
    run,
    time,
    sasm,
    dump,
};

struct Flag {
    FlagType type;
    std::optional<std::string> operand;
};

class ArgParser {
private:
    char** m_argv;
    int m_argc;
    std::vector<Flag> m_flags;
public:
    ArgParser(int argc, char** argv) {
        m_argc = argc;
        m_argv = argv;
    }
    std::optional<Flag> find_flag(FlagType type) {
        for(int i = 0;i < static_cast<int>(m_flags.size());++i) {
            if(m_flags[i].type == type) {
                return m_flags[i];
            }
        }
        return std::nullopt;
    }
    void parse() {
        for(int i = 2;i < m_argc;++i) {
            if(strcmp(m_argv[i], "-o") == 0) {
                if(i == (m_argc - 1)) {
                    std::cerr << "-o argument except file name\n";
                    exit(1);
                }
                m_flags.push_back({ .type = FlagType::output , .operand = m_argv[++i] });
            }
            else if(strcmp(m_argv[i], "-d") == 0) {
                if(i == (m_argc - 1)) {
                    std::cerr << "-d argument except file name\n";
                    exit(1);
                }
                m_flags.push_back({ .type = FlagType::dump , .operand = m_argv[++i] });
            }
            else if(strcmp(m_argv[i], "-r") == 0) {
                m_flags.push_back({ .type = FlagType::run , .operand = std::nullopt });
            }
            else if(strcmp(m_argv[i], "-t") == 0) {
                m_flags.push_back({ .type = FlagType::time , .operand = std::nullopt });
            }
            else if(strcmp(m_argv[i], "-s") == 0) {
                m_flags.push_back({ .type = FlagType::sasm , .operand = std::nullopt });
            }
            else {
                printf("unkown command flag `%s`\n", m_argv[i]);
                exit(1);
            }
        }
    }
    int compile() {
        if(auto d_flag = find_flag(FlagType::dump)) {
            return EXIT_SUCCESS;
        }
        system("nasm --gprefix _ -fwin32 output.asm -o output.o");
        if(auto s_flag = find_flag(FlagType::sasm)) {}
        else {
            system("del output.asm");
        }
        std::optional<Flag> has_o_flag = std::nullopt;
        if(auto o_flag = find_flag(FlagType::output)) {
            has_o_flag = o_flag;
            std::string link_com;
            assert(o_flag.value().operand.has_value());
            link_com = "gcc -o " + o_flag.value().operand.value() + " " + __PATH "/lib/lib_core.o output.o -m32";
            system(link_com.c_str());
        } else {
            system("gcc -o out.exe output.o " __PATH "/lib/lib_core.o -m32");
        }
        system("del output.o");
        if(auto r_flag = find_flag(FlagType::run)) {
            if(has_o_flag.has_value()) {
                return system(has_o_flag.value().operand.value().c_str());
            }
            else {
                return system("out.exe");
            }
        }
        return EXIT_SUCCESS;
    }
};