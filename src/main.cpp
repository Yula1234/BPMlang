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
#include <unordered_set>
#include <chrono>
#include <ctime>
#include <stdexcept>
#include <functional>
#include <random>
#include <deque>
#include <windows.h>

namespace fs = std::filesystem;

/*
    To start using compiller
    compile ./lib/lib_core.c
    with gcc (gcc -c ./lib/lib_core.c -o ./lib/lib_core.o -m32).
*/

#include "arena.hpp"


GString basepath;
bool __slashinpath;

#define __PATH "C:/cpp/BPMlang" /*
    path to compiller (bpm.exe).
    and standart libraries.
*/

#include "fasm.hpp"
#include "tcc_linker.hpp"
#include "argsparser.hpp"
#include "tokenization.hpp"
#include "diagnostic.hpp"
#include "datatype.hpp"
#include "ast.hpp"
#include "parser.hpp"
#include "sema.hpp"
#include "ir.hpp"
#include "ir_opt.hpp"
#include "generation.hpp"


int main(int argc, char* argv[]) {

    ArenaAllocator* global_allocator = g_GlobalArena;

    auto start_all = std::chrono::system_clock::now();

    ArgParser argparser(argc, argv);
    argparser.parse();

    GString input_filename = argparser.get_input_file();
    
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

    GString user_code;
    {
        GStringStream contents_stream;
        std::fstream input(input_filename.c_str(), std::ios::in);
        contents_stream << input.rdbuf();
        user_code = contents_stream.str();
    }

    auto start_lexing = std::chrono::system_clock::now();

    //Tokenizer internal_tokenizer(std::move(INTERNAL_CODE::IMPLEMENTATION));
    //auto internal_res = internal_tokenizer.tokenize("<built-in>");
    
    Tokenizer user_tokenizer(std::move(user_code));
    auto user_res = user_tokenizer.tokenize(input_filename);
    
    //internal_res.tokens->insert(internal_res.tokens->end(), user_res.tokens->begin(), user_res.tokens->end());

    auto end_lexing = std::chrono::system_clock::now();

    DiagnosticManager dmanager{};
    //dmanager.save_file("<built-in>", std::move(*internal_res.lines));
    dmanager.save_file(std::move(input_filename), std::move(*user_res.lines));

    auto start_parsing = std::chrono::system_clock::now();
    
    Parser parser(std::move(*user_res.tokens), &dmanager, global_allocator); 
    
    std::optional<NodeProg*> prog = parser.parse_prog();

    auto end_parsing = std::chrono::system_clock::now();

    if (!prog.has_value()) {
        std::cerr << "Invalid program" << std::endl;
        exit(EXIT_FAILURE);
    }

    auto start_generation = std::chrono::system_clock::now();

    SemanticContext sema(prog.value(), &dmanager, global_allocator);
    sema.analyze_prog();

    Generator generator(prog.value(), &dmanager, global_allocator, &sema);
    if(auto _f_opt = argparser.find_flag(FlagType::optimize)) {
        generator.m_optimize = true;
    }
    const GString& generated_asm = generator.gen_prog();

    auto end_generation = std::chrono::system_clock::now();

    if(auto _f_time = argparser.find_flag(FlagType::time)) {
        std::chrono::duration<double> elapsed_seconds = end_lexing-start_lexing;
        printf("Lexing took: %fs\n", elapsed_seconds.count());
        elapsed_seconds = end_parsing-start_parsing;
        printf("Parsing took: %fs\n", elapsed_seconds.count());
        elapsed_seconds = end_generation-start_generation;
        printf("Generation took: %fs\n", elapsed_seconds.count());
    }
    auto fexit_status = argparser.compile(start_all, generated_asm);
    return fexit_status;
}