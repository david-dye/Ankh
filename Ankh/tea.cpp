#include <iostream>
#include <string>
#include <vector>
#include <cstdint>
#include <algorithm>
#include <random>

const uint32_t g_max_nat_bits = 32;
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
    int tea_encrypt(nat* v1, nat* v2, nat* k1, nat* k2, nat* k3, nat* k4);
    int tea_decrypt(nat* v1, nat* v2, nat* k1, nat* k2, nat* k3, nat* k4, nat* ret1, nat* ret2);
}

int main() {
    // Create a random number generator
    std::mt19937 gen(42774277); // Seeded Mersenne Twister engine
    std::uniform_int_distribution<uint32_t> dist(0, UINT32_MAX);

    nat v1 = { dist(gen) };
    nat v2 = { dist(gen) };
    nat k1 = { dist(gen) };
    nat k2 = { dist(gen) };
    nat k3 = { dist(gen) };
    nat k4 = { dist(gen) };
    nat ret1 = {};
    nat ret2 = {};

    uint32_t v1_orig = v1.limbs[0];
    uint32_t v2_orig = v2.limbs[0];

    tea_encrypt(&v1, &v2, &k1, &k2, &k3, &k4);
    printf("Encryption of ");
    printf("%u and %u", v1_orig, v2_orig);
    printf(" is ");
    print_nat(v1);
    printf(" and ");
    print_nat(v2);
    printf(".\n");

    tea_decrypt(&v1, &v2, &k1, &k2, &k3, &k4, &ret1, &ret2);
    printf("Decryption of ");
    print_nat(v1);
    printf(" and ");
    print_nat(v2);
    printf(" is ");
    print_nat(ret1);
    printf(" and ");
    print_nat(ret2);
    printf(".\n");
}