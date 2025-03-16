g++ -std=c++20 -I$(brew --prefix llvm)/include -L$(brew --prefix llvm)/lib -o parser parser.cpp -lLLVM
