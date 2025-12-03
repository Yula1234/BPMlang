#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <optional>
#include <unordered_map>
#include <filesystem>
#include <cstdlib>
#include <cassert>
#include <algorithm>

namespace fs = std::filesystem;

enum class FlagType {
    output,
    run,
    time,
    sasm,
    dump,
    optimize,
    help,
    unknown
};

struct FlagDefinition {
    FlagType type;
    std::string short_name;
    std::string long_name;
    bool requires_value;
    std::string description;
};

struct ParsedFlag {
    FlagType type;
    std::optional<std::string> value;
};

std::pair<int, std::string> compileFasmSilent(const std::string& cmd) {
    std::string fullCmd = cmd + " 2>&1";
    std::string output;
    char buffer[128];
    FILE* pipe = _popen(fullCmd.c_str(), "r");

    if (!pipe) {
        return std::pair(1, output);
    }

    while (fgets(buffer, sizeof(buffer), pipe) != NULL) {
        output += buffer;
    }

    int returnCode = _pclose(pipe);

    return std::pair(returnCode, output);
}

class ArgParser {
private:
    int m_argc;
    char** m_argv;

    std::vector<FlagDefinition> m_defs;
    
    std::vector<ParsedFlag> m_parsed_flags;
    std::string m_input_file;
    std::string m_binary_name;

    void init_definitions() {
        m_defs = {
            {FlagType::output,   "-o", "--output",   true,  "Specify output file name"},
            {FlagType::dump,     "-d", "--dump",     true,  "Dump IR/ASM to file and exit"},
            {FlagType::run,      "-r", "--run",      false, "Run the program after compilation"},
            {FlagType::time,     "-t", "--time",     false, "Measure compilation time"},
            {FlagType::optimize, "-O", "--optimize", false, "Enable IR optimization"},
            {FlagType::sasm,     "-s", "--sasm",     false, "Keep intermediate .asm file"},
            {FlagType::help,     "-h", "--help",     false, "Show this help message"}
        };
    }

    std::optional<FlagDefinition> find_def(const std::string& arg) {
        for (const auto& def : m_defs) {
            if (def.short_name == arg || def.long_name == arg) {
                return def;
            }
        }
        return std::nullopt;
    }

public:
    ArgParser(int argc, char** argv) 
        : m_argc(argc), m_argv(argv) 
    {
        if (argc > 0) m_binary_name = argv[0];
        init_definitions();
    }
    std::optional<ParsedFlag> find_flag(FlagType type) {
        for (const auto& flag : m_parsed_flags) {
            if (flag.type == type) {
                return flag;
            }
        }
        return std::nullopt;
    }

    void print_help() {
        std::cout << "Usage: " << fs::path(m_binary_name).filename().string() << " <input.bpm> [options]\n\n";
        std::cout << "Options:\n";
        for (const auto& def : m_defs) {
            std::cout << "  " << def.short_name << ", " << def.long_name;
            if (def.requires_value) std::cout << " <value>";
            
            // Выравнивание
            size_t len = def.short_name.length() + def.long_name.length() + (def.requires_value ? 7 : 0);
            if (len < 20) {
                for(size_t i = 0; i < 20 - len; ++i) std::cout << " ";
            } else {
                std::cout << "\n                      ";
            }
            
            std::cout << " " << def.description << "\n";
        }
        std::cout << std::endl;
    }

    void parse() {
        
        bool input_found = false;

        for (int i = 1; i < m_argc; ++i) {
            std::string arg = m_argv[i];

            if (arg[0] == '-') {
                auto def_opt = find_def(arg);
                if (!def_opt.has_value()) {
                    std::cerr << "Error: Unknown flag `" << arg << "`\n";
                    std::cerr << "Use -h or --help for usage info.\n";
                    exit(EXIT_FAILURE);
                }
                
                FlagDefinition def = def_opt.value();
                std::optional<std::string> val = std::nullopt;

                if (def.requires_value) {
                    if (i + 1 < m_argc && m_argv[i+1][0] != '-') {
                        val = m_argv[++i];
                    } else {
                        std::cerr << "Error: Flag `" << arg << "` requires a value.\n";
                        exit(EXIT_FAILURE);
                    }
                }

                m_parsed_flags.push_back({def.type, val});

                if (def.type == FlagType::help) {
                    print_help();
                    exit(EXIT_SUCCESS);
                }
            } else {
                if (!input_found) {
                    m_input_file = arg;
                    input_found = true;
                } else {
                    std::cerr << "Warning: Multiple input files specified, using `" << m_input_file << "`, ignoring `" << arg << "`.\n";
                }
            }
        }

    }
    
    std::string get_input_file() const {
        return m_input_file;
    }

    int compile(auto start_comp) {
        if (find_flag(FlagType::dump)) {
            return EXIT_SUCCESS;
        }

        fs::path asm_file = "output.asm";
        fs::path obj_file = "output.o";

        auto start_fasm = std::chrono::system_clock::now();
        
        std::string fasm_cmd = "fasm " + asm_file.string() + " " + obj_file.string();
        auto fasm_res = compileFasmSilent(fasm_cmd);
        if(fasm_res.first != 0) {
            std::cerr << "Fasm error:\n";
            std::cerr << fasm_res.second << "\n";
            exit(EXIT_FAILURE);
        }

        auto end_fasm = std::chrono::system_clock::now();

        if(find_flag(FlagType::time)) {
            std::chrono::duration<double> fasm_elapsed_seconds = end_fasm-start_fasm;
            std::cout << "Fasm took: " << fasm_elapsed_seconds.count() << "s" << std::endl;
        }

        if (!find_flag(FlagType::sasm)) {
            std::error_code ec;
            fs::remove(asm_file, ec);
            if (ec) std::cerr << "Warning: Could not delete " << asm_file << ": " << ec.message() << "\n";
        }

        fs::path lib_core = fs::path(__PATH) / "lib" / "lib_core.o";
        fs::path output_exe = "out.exe";
        
        if (auto o_flag = find_flag(FlagType::output)) {
            output_exe = o_flag->value.value();
        }

        if (!fs::exists(lib_core)) {
            std::cerr << "Error: Core library not found at " << lib_core << "\n";
            std::cerr << "Please check __PATH definition.\n";
            return EXIT_FAILURE;
        }

        std::string link_cmd = "gcc -o " + output_exe.string() + " " + obj_file.string() + " " + lib_core.string() + " -m32";
        
        if (system(link_cmd.c_str()) != 0) {
            std::cerr << "Error: Linker (gcc) failed.\n";
            return EXIT_FAILURE;
        }

        std::error_code ec;
        fs::remove(obj_file, ec);

        auto end_comp = std::chrono::system_clock::now();

        if(find_flag(FlagType::time)) {
            std::chrono::duration<double> elapsed_seconds = end_comp-start_comp;
            std::cout << "\nThe whole compilation took: " << elapsed_seconds.count() << "s" << std::endl;
        }

        if (find_flag(FlagType::run)) {
            int run_ret = system(output_exe.string().c_str());
            return run_ret;
        }

        return EXIT_SUCCESS;
    }
};