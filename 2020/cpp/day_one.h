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
        auto range = input_strings | std::ranges::views::transform([](const std::string& string) { return std::atoi(string.c_str()); });
        input = std::vector(range.begin(), range.end());
    }

    void part_one() const {
        auto begin = std::chrono::high_resolution_clock::now();
        for (int i = 0; i < input.size(); ++i) {
            for (int j = 0; j < i; ++j) {
                if (input[i] + input[j] == target) {
                    auto end = std::chrono::high_resolution_clock::now();
                    fmt::print("The answer for day one part one is {}\n", input[i] * input[j]);
                    fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - begin).count());
                    return;
                }
            }
        }
    }

    void part_two() const {
        auto begin = std::chrono::high_resolution_clock::now();

        for (int i = 0; i < input.size(); ++i) {
            for (int j = 0; j < i; ++j) {
                for (int k = 0; k < j; ++k) {
                    if (input[i] + input[j] + input[k] == target) {
                        auto end = std::chrono::high_resolution_clock::now();
                        fmt::print("The answer for day one part two is {}\n", input[i] * input[j] * input[k]);
                        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - begin).count());
                        return;
                    }
                }
            }
        }
    }
};
