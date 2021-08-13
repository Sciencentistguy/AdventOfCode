#include "day_01.h"

#include <range/v3/all.hpp>

day_01::day_01() {
    auto input_strings = readFile("Inputs/day_01.txt");
    auto start = std::chrono::high_resolution_clock::now();
    auto range = input_strings | ranges::views::transform([](const std::string& string) { return static_cast<int>(fast_atol(string.c_str())); });
    input = std::vector(range.begin(), range.end());
    auto end = std::chrono::high_resolution_clock::now();
    fmt::print("Parsing input for day one took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}

void day_01::part_one() const {
    const auto start = std::chrono::high_resolution_clock::now();
    for (size_t i = 0; i < input.size(); ++i) {
        for (size_t j = 0; j < i; ++j) {
            if (input[i] + input[j] == target) {
                const auto result = input[i] * input[j];
                const auto end = std::chrono::high_resolution_clock::now();
                fmt::print("The answer for day one part one is {}\n", result);
                fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
                return;
            }
        }
    }
}

void day_01::part_two() const {
    const auto start = std::chrono::high_resolution_clock::now();
    for (size_t i = 0; i < input.size(); ++i) {
        for (size_t j = 0; j < i; ++j) {
            for (size_t k = 0; k < j; ++k) {
                if (input[i] + input[j] + input[k] == target) {
                    const auto result = input[i] * input[j] * input[k];
                    const auto end = std::chrono::high_resolution_clock::now();
                    fmt::print("The answer for day one part two is {}\n", result);
                    fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
                    return;
                }
            }
        }
    }
}
