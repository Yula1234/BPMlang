@echo off

echo [INFO] build src/main.cpp
g++ -O3 -flto=8 -fomit-frame-pointer -Os -Ofast -march=native -mtune=native -std=c++2a -fconcepts -fmax-errors=2 -Wno-maybe-uninitialized -Wno-uninitialized -Wdouble-promotion -Wdiv-by-zero -Wold-style-cast -Wextra -pedantic -Wall -Werror -Wswitch src/main.cpp fasm_lib.o libtcc.a -o bpm.exe