#include <iostream>
#include <chrono>
#include <string>
#include <vector>
#include <cstdint>
#include <algorithm>
#include <random>
#include <numeric>
#include <boost/multiprecision/cpp_int.hpp>

const uint32_t g_max_nat_bits = 1<<9;
const uint32_t num_limbs = g_max_nat_bits / 32;
using namespace boost::multiprecision;

typedef struct {
    uint32_t limbs[num_limbs];
} nat;

extern "C" {
    int tea_encrypt_no_opt(nat* v1, nat* v2, nat* k1, nat* k2, nat* k3, nat* k4);
    int tea_decrypt_no_opt(nat* v1, nat* v2, nat* k1, nat* k2, nat* k3, nat* k4, nat* ret1, nat* ret2);
}


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

cpp_int get_random(std::mt19937* gen, uint32_t bits) {
    cpp_int random_val = 0;
    for (int i = 0; i < bits/64; ++i) {
        cpp_int part = (*gen)();
        random_val <<= 64;
        random_val |= part;
    }
    return random_val;
}

void fill_nat(nat* n, cpp_int* c) {
    for (int i = 0; i < num_limbs; ++i) {
        unsigned int limb = (unsigned int)((*c) & 0xFFFFFFFF);
        n->limbs[i] = limb;
        *c >>= 32;
    }
}

void fill_nat_random(nat* n, std::mt19937* gen) {
    cpp_int random_val = get_random(gen, g_max_nat_bits);
    fill_nat(n, &random_val);
}

int main() {
    // Create a random number generator
    int seed = 42774277;
    std::mt19937 gen(seed); // Seeded Mersenne Twister engine
    std::uniform_int_distribution<uint32_t> dist(0, UINT32_MAX);
    // int num_iter = 100;

    
    // std::vector<double> total_time_opt
    // std::vector<double> total_time_no_opt;
    // for (int i = 0; i < 1; ++i) {
    nat v1 = {};
    nat v2 = {};
    nat k1 = {};
    nat k2 = {};
    nat k3 = {};
    nat k4 = {};
    fill_nat_random(&v1, &gen);
    fill_nat_random(&v2, &gen);
    fill_nat_random(&k1, &gen);
    fill_nat_random(&k2, &gen);
    fill_nat_random(&k3, &gen);
    fill_nat_random(&k4, &gen);
    nat ret1 = {};
    nat ret2 = {};

    uint32_t v1_orig = v1.limbs[0];
    uint32_t v2_orig = v2.limbs[0];

    auto start = std::chrono::high_resolution_clock::now();
    // tea_encrypt(&v1, &v2, &k1, &k2, &k3, &k4);
    // tea_decrypt(&v1, &v2, &k1, &k2, &k3, &k4, &ret1, &ret2);
    tea_encrypt_no_opt(&v1, &v2, &k1, &k2, &k3, &k4);
    tea_decrypt_no_opt(&v1, &v2, &k1, &k2, &k3, &k4, &ret1, &ret2);
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed_opt = end - start;
    std::cout << elapsed_opt.count() << "\n";
    // total_time_opt.push_back(elapsed_opt.count());

    // v1 = { dist(gen) };
    // v2 = { dist(gen) };
    // k1 = { dist(gen) };
    // k2 = { dist(gen) };
    // k3 = { dist(gen) };
    // k4 = { dist(gen) };
    // ret1 = {};
    // ret2 = {};
    // start = std::chrono::high_resolution_clock::now();
    // tea_encrypt_no_opt(&v1, &v2, &k1, &k2, &k3, &k4);
    // tea_decrypt_no_opt(&v1, &v2, &k1, &k2, &k3, &k4, &ret1, &ret2);
    // end = std::chrono::high_resolution_clock::now();
    // std::chrono::duration<double> elapsed_no_opt = end - start;
    // std::cout << "Elapsed time no opt: " << elapsed_no_opt.count() << " seconds\n";
    // total_time_no_opt.push_back(elapsed_no_opt.count());
    // }
    // double avg_time_opt = std::accumulate(total_time_opt.begin(), total_time_opt.end(), 0.0) / 100;
    // double avg_time_no_opt = std::accumulate(total_time_no_opt.begin(), total_time_no_opt.end(), 0.0) / 100;
    // std::cout << "\n\n\nAvg time opt: " << avg_time_opt << " seconds\n";
    // std::cout << "\n\n\nAvg time no opt: " << avg_time_no_opt << " seconds\n";
}

