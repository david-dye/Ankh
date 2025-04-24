#include <iostream>
#include <chrono>

extern "C" {
    int fib(int);
}

int main() {
    int n = 40;
	auto start = std::chrono::high_resolution_clock::now();
	int result = fib(n);
	auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;

    std::cout << "fib(" << n << "): " << result << std::endl;
    std::cout << "Elapsed time: " << elapsed.count() << " seconds\n";}

