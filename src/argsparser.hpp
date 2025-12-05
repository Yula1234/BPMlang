#pragma once

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
    GString short_name;
    GString long_name;
    bool requires_value;
    GString description;
};

struct ParsedFlag {
    FlagType type;
    std::optional<GString> value;
};

bool runProcess(const std::string& cmd) {
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    GVector<char> cmdBuffer(cmd.begin(), cmd.end());
    cmdBuffer.push_back(0);

    if (!CreateProcessA(NULL, cmdBuffer.data(), NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi)) {
        return false;
    }

    WaitForSingleObject(pi.hProcess, INFINITE);
    
    DWORD exitCode;
    GetExitCodeProcess(pi.hProcess, &exitCode);

    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    return exitCode == 0;
}

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

GString generate_temp_filename(const char* extension) {
    static std::random_device rd;
    static std::mt19937 gen(rd());
    static std::uniform_int_distribution<> dis(0, 15);
    
    GStringStream ss;
    ss << std::hex;
    for (int i = 0; i < 8; ++i) ss << dis(gen);
    ss << extension;
    
    return (fs::temp_directory_path() / ss.str()).string().c_str();
}

class ArgParser {
private:
    int m_argc;
    char** m_argv;

    GVector<FlagDefinition> m_defs;
    
    GVector<ParsedFlag> m_parsed_flags;
    GString m_input_file;
    GString m_binary_name;

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

    std::optional<FlagDefinition> find_def(const GString& arg) {
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
            GString arg = m_argv[i];

            if (arg[0] == '-') {
                auto def_opt = find_def(arg);
                if (!def_opt.has_value()) {
                    std::cerr << "Error: Unknown flag `" << arg << "`\n";
                    std::cerr << "Use -h or --help for usage info.\n";
                    exit(EXIT_FAILURE);
                }
                
                FlagDefinition def = def_opt.value();
                std::optional<GString> val = std::nullopt;

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
    
    GString get_input_file() const {
        return m_input_file;
    }

    int compile(auto start_comp, const GString& generated_asm) {

        bool timeflag = find_flag(FlagType::time).has_value();

        fs::path asm_file = "output.asm";
        fs::path obj_file = generate_temp_filename(".o").c_str();


        auto start_fasm = std::chrono::system_clock::now();
        if (!static_fasm::compile_via_fasm(generated_asm, obj_file)) {
            return EXIT_FAILURE;
        }
        auto end_fasm = std::chrono::system_clock::now();

        if(timeflag) {
            std::chrono::duration<double> fasm_elapsed_seconds = end_fasm-start_fasm;
            printf("Fasm took: %fs\n", fasm_elapsed_seconds.count());
        }


        if (find_flag(FlagType::sasm)) {
            std::fstream file("output.asm", std::ios::out);
            file << generated_asm;
        }

        fs::path lib_core = fs::path(__PATH) / "lib" / "lib_core.o";
        fs::path output_exe = "out.exe";
        
        if (auto o_flag = find_flag(FlagType::output)) {
            output_exe = o_flag->value.value();
        }


        auto start_link = std::chrono::system_clock::now();

        fs::path bin_dir = fs::path(__PATH); 
        fs::path tcc_lib_dir = fs::absolute(bin_dir / "lib" / "tcc");

        std::string tcc_lib_str = fs::absolute(tcc_lib_dir).string();

        static TccLinker linker;
        linker.init(tcc_lib_str);

        GVector<GString> input_objs;
        input_objs.push_back(GString(obj_file.string().c_str()));
        input_objs.push_back(GString(lib_core.string().c_str()));
        
        if (!linker.link(output_exe.string(), input_objs)) {
            std::cerr << "ERROR: Linking failed.\n";
            fs::remove(obj_file); 
            return EXIT_FAILURE;
        }

        auto end_link = std::chrono::system_clock::now();



        if(timeflag) {
            std::chrono::duration<double> link_elapsed_seconds = end_link-start_link;
            printf("Linking took: %fs\n", link_elapsed_seconds.count());
        }

        std::error_code ec;
        fs::remove(obj_file, ec);

        auto end_comp = std::chrono::system_clock::now();

        if(timeflag) {
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