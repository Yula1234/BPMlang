@echo off

echo Compiling src/main.cpp...
g++ -std=c++2a -fmax-errors=2 -Wdouble-promotion -Wdiv-by-zero -Wold-style-cast -Wextra -pedantic -Wall -Werror -Wswitch src/main.cpp -o bpm.exe