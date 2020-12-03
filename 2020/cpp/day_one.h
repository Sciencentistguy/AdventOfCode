#pragma once

#include <chrono>
#include <vector>

#include <fmt/core.h>

#include "common.h"

struct day_one {
    std::vector<int> input;
    static constexpr int target = 2020;

    day_one() {
        auto input_strings = readFile("Inputs/day_one.txt");
        auto start = std::chrono::high_resolution_clock::now();
        auto range = input_strings | std::ranges::views::transform([](const std::string& string) { return std::atoi(string.c_str()); });
        input = std::vector(range.begin(), range.end());
        auto end = std::chrono::high_resolution_clock::now();
        fmt::print("Parsing input for day one took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_one() const {
        const auto start = std::chrono::high_resolution_clock::now();
        for (int i = 0; i < input.size(); ++i) {
            for (int j = 0; j < i; ++j) {
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

    void part_two() const {
        const auto start = std::chrono::high_resolution_clock::now();
        for (int i = 0; i < input.size(); ++i) {
            for (int j = 0; j < i; ++j) {
                for (int k = 0; k < j; ++k) {
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
};
