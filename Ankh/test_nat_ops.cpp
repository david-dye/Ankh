
#include <iostream>
#include <string>
#include <vector>
#include <cstdint>
#include <algorithm>

const uint32_t g_max_nat_bits = 256;
const uint32_t num_limbs = g_max_nat_bits / 32;

typedef struct {
    uint32_t limbs[num_limbs];
} nat;


// Helper: compares if nat is zero
bool is_zero(nat& a) {
    for (uint32_t i = 0; i < num_limbs; ++i) {
        if (a.limbs[i] != 0) return false;
    }
    return true;
}

// Helper: right shift by 1 bit (divides by 2)
void right_shift(nat& a) {
    uint32_t carry = 0;
    for (long i = num_limbs - 1; i >= 0; --i) {
        uint32_t next = a.limbs[i];
        a.limbs[i] = (next >> 1) | (carry << 31);
        carry = next & 1;
    }
}

// Helper: get remainder when dividing by 10
uint32_t div_by_10(nat& a) {
    uint64_t rem = 0;
    for (long i = num_limbs - 1; i >= 0; --i) {
        uint64_t cur = (rem << 32) | a.limbs[i];
        a.limbs[i] = static_cast<uint32_t>(cur / 10);
        rem = cur % 10;
    }
    return static_cast<uint32_t>(rem);
}

// prints a nat by converting it to a string
void print_nat(nat& input) {
    if (is_zero(input)) {
        std::cout << "0";
        return;
    }

    nat temp = input; // Copy so we can modify
    std::string result;

    while (!is_zero(temp)) {
        uint32_t digit = div_by_10(temp);
        result += static_cast<char>('0' + digit);
    }

    std::reverse(result.begin(), result.end());
    std::cout << result;
}


extern "C" {
    void max(nat* ret, nat* a, nat* b);
    void add(nat* ret, nat* a, nat* b);
    void add10(nat* ret, nat* a, nat* b);
    void sub(nat* ret, nat* a, nat* b);
    void assign(nat* ret, nat* a);
    int add_in_place(nat* a, nat* b);
}

int main() {
    nat a = {};
    nat b = {};
    nat ret = {};
    a.limbs[0] = 80;
    a.limbs[3] = 689;
    b.limbs[0] = 41;
    b.limbs[1] = 7;
    
    max(&ret, &a, &b);
    printf("Maximum between ");
    print_nat(a);
    printf(" and ");
    print_nat(b);
    printf(" is ");
    print_nat(ret);
    printf(".\n");

    add(&ret, &a, &b);
    printf("Sum of ");
    print_nat(a);
    printf(" and ");
    print_nat(b);
    printf(" is ");
    print_nat(ret);
    printf(".\n");

    add10(&ret, &a, &b);
    printf("Sum + 10 of ");
    print_nat(a);
    printf(" and ");
    print_nat(b);
    printf(" is ");
    print_nat(ret);
    printf(".\n");

    sub(&ret, &a, &b);
    printf("Difference between ");
    print_nat(a);
    printf(" and ");
    print_nat(b);
    printf(" is ");
    print_nat(ret);
    printf(".\n");

    assign(&ret, &a);
    printf("Correct assignment should copy a = ");
    print_nat(a);
    printf(" here: ");
    print_nat(ret);
    printf(".\n");

    printf("Sum, returned by reference, of ");
    print_nat(a);
    printf(" and ");
    print_nat(b);
    printf(" is ");
    int x = add_in_place(&a, &b);
    print_nat(a);
    printf(".\n");
}

