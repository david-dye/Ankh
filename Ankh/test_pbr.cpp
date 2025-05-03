#include <iostream>

extern "C" {
    int foo(int& a);
}

int main() {
    int a = 0;
    printf("a before foo: %i\n", a);
    foo(a);
    printf("a after foo: %i", a);
}

