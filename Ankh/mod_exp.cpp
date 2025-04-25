#include <iostream>

extern "C" {
    int mod_exp(int a, int b, int n);
}

int main() {
    int a = 649;
    int b = 550;
    int n = 37;
    printf(
        "mod_exp(%i, %i, %i): %i",
        a, b, n, mod_exp(a, b, n)
    );
}

