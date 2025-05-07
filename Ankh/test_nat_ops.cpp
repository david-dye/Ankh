
#include <iostream>
#include <string>
#include <vector>
#include <cstdint>
#include <algorithm>
#include <random>

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
    void mul(nat* ret, nat* a, nat* b);
    void mod(nat* ret, nat* a, nat* b);
    void assign(nat* ret, nat* a);
    int add_in_place(nat* a, nat* b);
    void bsl(nat* ret, nat* a, int b);
    void bsr(nat* ret, nat* a, int b);
    void bwxor(nat* ret, nat* a, nat* b);
    void bwor(nat* ret, nat* a, nat* b);
    void bwand(nat* ret, nat* a, nat* b);
}

int main() {
    nat a = {};
    nat b = {};
    nat ret = {};
    //a.limbs[0] = 42;
    //a.limbs[3] = 689;
    //b.limbs[0] = 42;
    //b.limbs[1] = 7;
    std::mt19937 gen(427742); // Seeded Mersenne Twister engine
    std::uniform_int_distribution<uint32_t> dist(0, UINT32_MAX);

    for (int i = 0; i < num_limbs; ++i) {
        a.limbs[i] = dist(gen);
        b.limbs[i] = dist(gen);
    }
    
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

    mul(&ret, &a, &b);
    printf("Product of ");
    print_nat(a);
    printf(" and ");
    print_nat(b);
    printf(" is ");
    print_nat(ret);
    printf(".\n");

    mod(&ret, &a, &b);
    printf("Modulus: ");
    print_nat(a);
    printf(" %% ");
    print_nat(b);
    printf(" = ");
    print_nat(ret);
    printf(".\n");

    assign(&ret, &a);
    printf("Correct assignment should copy a = ");
    print_nat(a);
    printf(" here: ");
    print_nat(ret);
    printf(".\n");

    int bs_op = 40;
    bsl(&ret, &a, bs_op);
    printf("Bitshift left of ");
    print_nat(a);
    printf(" by %i : ", bs_op);
    print_nat(ret);
    printf(".\n");

    bsr(&ret, &a, bs_op);
    printf("Bitshift right of ");
    print_nat(a);
    printf(" by %i : ", bs_op);
    print_nat(ret);
    printf(".\n");

    bwxor(&ret, &a, &b);
    printf("Bitwise Xor of ");
    print_nat(a);
    printf(" and ");
    print_nat(b);
    printf(" is ");
    print_nat(ret);
    printf(".\n");

    bwor(&ret, &a, &b);
    printf("Bitwise Or of ");
    print_nat(a);
    printf(" and ");
    print_nat(b);
    printf(" is ");
    print_nat(ret);
    printf(".\n");

    bwand(&ret, &a, &b);
    printf("Bitwise And of ");
    print_nat(a);
    printf(" and ");
    print_nat(b);
    printf(" is ");
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

