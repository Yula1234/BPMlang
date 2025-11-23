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

#include "libs/binary_tree.hpp"
#include "arena.hpp"
#include "argsparser.hpp"
#include "generation.hpp"

void show_usage(std::ostream& stream) {
    stream << "Incorrect usage. Correct usage is..." << std::endl;
    stream << "bpm <input.bpm> <flags>" << std::endl;
}

int main(int argc, char* argv[]) {

    auto start = std::chrono::system_clock::now();

    if (argc < 2) {
        show_usage(std::cerr);
        return EXIT_FAILURE;
    }

    basepath = argv[1];

    __slashinpath = basepath.find("/") != std::string::npos;

    if(__slashinpath) {
        while(basepath[basepath.length() - 1] != '/') {
            basepath.pop_back();
        }
        basepath.pop_back();
    }

    if(FILE *file = fopen(argv[1], "r")) {
        fclose(file);
    } else {
        char* path = argv[1];
        std::cerr << "ERROR: file `" << path << "` not found\n";
        exit(1);
    }

    ArgParser argparser(argc, argv);

    argparser.parse();

    std::string contents;
    {
        std::stringstream contents_stream;
        std::fstream input(argv[1], std::ios::in);
        contents_stream << input.rdbuf();
        contents = contents_stream.str();
    }

    std::string user_code;
    {
        std::stringstream contents_stream;
        std::fstream input(argv[1], std::ios::in);
        contents_stream << input.rdbuf();
        user_code = contents_stream.str();
    }

    Tokenizer internal_tokenizer(std::move(INTERNAL_CODE::IMPLEMENTATION));
    auto internal_res = internal_tokenizer.tokenize("<built-in>");
    
    Tokenizer user_tokenizer(std::move(user_code));
    auto user_res = user_tokenizer.tokenize(argv[1]);
    
    internal_res.tokens->insert(internal_res.tokens->end(), user_res.tokens->begin(), user_res.tokens->end());
    
    Parser parser(std::move(*internal_res.tokens), std::vector<std::string>()); 
    
    parser.m_lines["<built-in>"] = std::move(*internal_res.lines);
    parser.m_lines[argv[1]]       = std::move(*user_res.lines);
    
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