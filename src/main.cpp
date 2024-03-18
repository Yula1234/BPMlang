#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <vector>
#include <cstdio>

#include "generation.hpp"

void usage(std::ostream& stream) {
    stream << "Incorrect usage. Correct usage is..." << std::endl;
    stream << "bpm <input.bpm>" << std::endl;
}

int main(int argc, char* argv[])
{
    if (argc != 2) {
        usage(std::cerr);
        return EXIT_FAILURE;
    }

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
        std::fstream file("output.asm", std::ios::out);
        const std::string& generated_asm = generator.gen_prog();
        file << generated_asm;
    }

    system("nasm --gprefix _ -fwin32 output.asm -o output.o");
    system("ld -o out.exe output.o -l kernel32");
    system("del output.o");

    return EXIT_SUCCESS;
}