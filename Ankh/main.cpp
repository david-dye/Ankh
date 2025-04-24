#include <iostream>

extern "C" {
    int f(int, int);
}

int main() {
    std::cout << "f(5): " << f(5, 4) << std::endl;
}

