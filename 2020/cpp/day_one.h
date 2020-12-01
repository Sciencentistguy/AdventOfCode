#pragma once

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
        for (auto i : input) {
            for (auto j : input) {
                if (i + j == target) {
                    fmt::print("The answer for day one part one is {}\n", i * j);
                    return;
                }
            }
        }
    }

    void part_two() const {
        for (auto i : input) {
            for (auto j : input) {
                for (auto k : input) {
                    if (i + k + j == target) {
                        fmt::print("The answer for day one part two is {}\n", i * j * k);
                        return;
                    }
                }
            }
        }
    }
};
