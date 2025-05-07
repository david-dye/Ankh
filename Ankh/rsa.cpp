#include <iostream>
#include <string>
#include <vector>
#include <cstdint>
#include <algorithm>
#include <random>
#include <chrono>
#include <fstream>

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
    void mod_exp_no_opt(nat* ret, nat* a, nat* b, nat* c);
    void mod_exp_opt(nat* ret, nat* a, nat* b, nat* c);
}

void randomize_nat(nat* v, std::mt19937& gen, std::uniform_int_distribution<uint32_t>& dist) {
    //we populate up to num_limbs/2 since multiplication requires the upper half of the data
    for (int i = 0; i < num_limbs / 2; ++i) {
        v->limbs[i] = dist(gen);
    }
}

int main() {
    // Create a random number generator
    std::mt19937 gen(777); // Seeded Mersenne Twister engine
    std::uniform_int_distribution<uint32_t> dist(0, UINT32_MAX);

    nat a = {};
    nat b = {};
    nat c = {};
    nat ret = {};

    //const int num_iters = 10000;
    //uint64_t times_no_opt[num_iters];
    //uint64_t times_opt[num_iters];

    //uint64_t total_time_opt = 0;
    //for (int i = 0; i < num_iters; ++i) {
    //    randomize_nat(&a, gen, dist);
    //    randomize_nat(&b, gen, dist);
    //    randomize_nat(&c, gen, dist);
    //    auto start_opt = std::chrono::high_resolution_clock::now();
    //    mod_exp_opt(&ret, &a, &b, &c);
    //    auto end_opt = std::chrono::high_resolution_clock::now();
    //    uint64_t duration_opt = std::chrono::duration_cast<std::chrono::microseconds>(end_opt - start_opt).count();
    //    times_opt[i] = duration_opt;
    //    total_time_opt += duration_opt;
    //}
    //uint64_t avg_time_opt = total_time_opt / num_iters;
    //printf("mod_exp_opt result: %lld microseconds\n", avg_time_opt);


    //uint64_t total_time_no_opt = 0;
    //for (int i = 0; i < num_iters; ++i) {
    //    randomize_nat(&a, gen, dist);
    //    randomize_nat(&b, gen, dist);
    //    randomize_nat(&c, gen, dist);
    //    auto start_no_opt = std::chrono::high_resolution_clock::now();
    //    mod_exp_no_opt(&ret, &a, &b, &c);
    //    auto end_no_opt = std::chrono::high_resolution_clock::now();
    //    uint64_t duration_no_opt = std::chrono::duration_cast<std::chrono::microseconds>(end_no_opt - start_no_opt).count();
    //    times_no_opt[i] = duration_no_opt;
    //    total_time_no_opt += duration_no_opt;
    //}
    //uint64_t avg_time_no_opt = total_time_no_opt / num_iters;
    //printf("mod_exp_no_opt result: %lld microseconds\n", avg_time_no_opt);

    //std::ofstream out_file("timing_results_opt_512_fixedvfixed.csv");
    //if (!out_file) {
    //    std::cerr << "Failed to open file for writing.\n";
    //    return 1;
    //}

    //out_file << "no_opt,opt\n";

    //for (int i = 0; i < num_iters; ++i) {
    //    out_file << times_no_opt[i] << "," << times_opt[i] << "\n";
    //}

    //out_file.close();
    //std::cout << "Timing results written to timing_results.csv\n";

    //Correctness test

    randomize_nat(&a, gen, dist);
    randomize_nat(&b, gen, dist);
    randomize_nat(&c, gen, dist);
    mod_exp_opt(&ret, &a, &b, &c);
    printf("a^b mod c with a = ");
    print_nat(a);
    printf(" , b = ");
    print_nat(b);
    printf(" , and c = ");
    print_nat(c);
    printf(" is: ");
    print_nat(ret);
    printf(".\n");
}