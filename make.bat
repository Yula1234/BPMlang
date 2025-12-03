@echo off

echo [INFO] build src/main.cpp
g++ -O3 -flto=4 -Os -Ofast -std=c++2a -fconcepts -fmax-errors=2 -Wdouble-promotion -Wdiv-by-zero -Wold-style-cast -Wextra -pedantic -Wall -Werror -Wswitch src/main.cpp -o bpm.exe