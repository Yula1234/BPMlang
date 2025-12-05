@echo off

echo [INFO] build src/main.cpp
g++ -O2 -ggdb -std=c++2a -fconcepts -fmax-errors=2 -Wno-return-type src/main.cpp -o bpm.exe