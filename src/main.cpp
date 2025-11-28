#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <cstring>
#include <cstddef>
#include <memory>
#include <utility>
#include <variant>
#include <filesystem>
#include <algorithm>
#include <unordered_map>
#include <set>
#include <chrono>
#include <ctime>
#include <stdexcept>
#include <functional>

/*
    To start using compiller
    compile ./lib/lib_core.c
    with gcc (gcc -c ./lib/lib_core.c -o ./lib/lib_core.o -m32).
*/

std::string basepath;
bool __slashinpath;

#define __map std::unordered_map
#define __stdset std::set
#define __stdvec std::vector

#define __PATH "C:/cpp/BPMlang" /*
    path to compiller (bpm.exe).
    and standart libraries.
*/

#include "arena.hpp"
#include "argsparser.hpp"
#include "generation.hpp"

void show_usage(std::ostream& stream) {
    stream << "Incorrect usage. Correct usage is..." << std::endl;
    stream << "bpm <input.bpm> <flags>" << std::endl;
}

int main(int argc, char* argv[]) {

    auto start = std::chrono::system_clock::now();

    ArgParser argparser(argc, argv);
    argparser.parse();

    std::string input_filename = argparser.get_input_file();
    
    if (input_filename.empty()) {
        std::cerr << "ERROR: No input file provided.\n";
        argparser.print_help();
        return EXIT_FAILURE;
    }

    basepath = input_filename;

    if (!std::filesystem::exists(input_filename)) {
        std::cerr << "ERROR: file `" << input_filename << "` not found\n";
        exit(1);
    }

    std::string user_code;
    {
        std::stringstream contents_stream;
        std::fstream input(input_filename, std::ios::in);
        contents_stream << input.rdbuf();
        user_code = contents_stream.str();
    }

    Tokenizer internal_tokenizer(std::move(INTERNAL_CODE::IMPLEMENTATION));
    auto internal_res = internal_tokenizer.tokenize("<built-in>");
    
    Tokenizer user_tokenizer(std::move(user_code));
    auto user_res = user_tokenizer.tokenize(input_filename);
    
    internal_res.tokens->insert(internal_res.tokens->end(), user_res.tokens->begin(), user_res.tokens->end());
    
    Parser parser(std::move(*internal_res.tokens), std::vector<std::string>()); 
    
    parser.m_lines["<built-in>"] = std::move(*internal_res.lines);
    parser.m_lines[input_filename]       = std::move(*user_res.lines);
    
    std::optional<NodeProg*> prog = parser.parse_prog();

    if (!prog.has_value()) {
        std::cerr << "Invalid program" << std::endl;
        exit(EXIT_FAILURE);
    }

    {
        Generator generator(prog.value());
        if(auto _f_time = argparser.find_flag(FlagType::optimize)) {
            generator.m_optimize = true;
        }
        generator.get_props_from_parser(&parser);
        const std::string& generated_asm = generator.gen_prog();
        std::fstream file("output.asm", std::ios::out);
        file << generated_asm;
    }

    if(auto _f_time = argparser.find_flag(FlagType::time)) {
        auto end = std::chrono::system_clock::now();
        std::chrono::duration<double> elapsed_seconds = end-start;
        std::cout << "Compilation took: " << elapsed_seconds.count() << "s" << std::endl;
    }
    auto fexit_status = argparser.compile();
    return fexit_status;
}