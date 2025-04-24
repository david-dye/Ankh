#include <iostream>

extern "C" {
    int foo();
}

int main() {
    std::cout << "foo(): " << foo() << std::endl;
}

