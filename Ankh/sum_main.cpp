#include <iostream>
#include <chrono>

extern "C" {
    int bar();
}

int main() {
    int n = 40;
	auto start = std::chrono::high_resolution_clock::now();
	int result = bar();
	auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;

    std::cout << "fib(" << n << "): " << result << std::endl;
    std::cout << "Elapsed time: " << elapsed.count() << " seconds\n";
}

