#include <iostream>

extern "C" {
    int mod_exp(int a, int b, int n);
}

int main() {
    int a = 649;
    int b = 550;
    int n = 37;
    std::cout << "mod_exp(a, b, c): " << mod_exp(a, b, n) << std::endl;
}

