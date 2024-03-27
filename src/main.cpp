#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <vector>
#include <cstdio>
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

#include "argsparser.hpp"
#include "generation.hpp"

void usage(std::ostream& stream) {
    stream << "Incorrect usage. Correct usage is..." << std::endl;
    stream << "bpm <input.bpm> <flags>" << std::endl;
}

int main(int argc, char* argv[])
{
    if (argc < 2) {
        usage(std::cerr);
        return EXIT_FAILURE;
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

    Tokenizer tokenizer(std::move(contents));
    std::vector<Token> tokens = tokenizer.tokenize(argv[1]);

    Parser parser(std::move(tokens));
    std::optional<NodeProg> prog = parser.parse_prog();

    if (!prog.has_value()) {
        std::cerr << "Invalid program" << std::endl;
        exit(EXIT_FAILURE);
    }

    {
        Generator generator(prog.value());
        generator.get_props_from_parser(&parser);
        std::fstream file("output.asm", std::ios::out);
        const std::string& generated_asm = generator.gen_prog();
        file << generated_asm;
    }

    return argparser.compile();
}