#pragma once

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <numeric>
#include <ranges>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <ctre.hpp>
#include <fmt/core.h>

#include "common.h"
#include "computer.h"

struct day_nine {
    std::vector<int64_t> input;
    int64_t part_one_answer;

    day_nine() {
        auto input_strings{readFile("Inputs/day_nine.txt")};
        const auto start = std::chrono::high_resolution_clock::now();
        for (const auto& line : input_strings) {
            input.push_back(fast_atol(line.c_str()));
        }
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("Parsing input for day nine took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    static constexpr bool sumOfPairs(int64_t target, int64_t* begin, size_t length = 25) {
        std::span<int64_t> range{begin, length};
        for (auto i : range) {
            for (auto j : range) {
                if (i + j == target) {
                    return true;
                }
            }
        }
        return false;
    }

    static constexpr const int64_t* sequenceOfNumbersSum(int64_t target, const int64_t* begin) {
        int64_t sum{0};
        while (sum < target) {
            sum += *begin++;
        }
        if (sum == target) {
            return begin;
        }
        return nullptr;
    }

    void part_one() {
        const auto start = std::chrono::high_resolution_clock::now();
        constexpr int offset{25};
        for (int i = offset; i < input.size(); ++i) {
            if (!sumOfPairs(input[i], &input[i - offset])) {
                const auto end = std::chrono::high_resolution_clock::now();
                fmt::print("The answer for day nine part one is {}\n", input[i]);
                fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
                part_one_answer = input[i];
                return;
            }
        }
    }

    void part_two() const {
        const auto start = std::chrono::high_resolution_clock::now();
        for (const auto& i : input) {
            auto* beginptr = &i;
            auto* endptr = sequenceOfNumbersSum(part_one_answer, beginptr);
            if (endptr) {
                auto max = *std::max_element(beginptr, endptr);
                auto min = *std::min_element(beginptr, endptr);
                const auto end = std::chrono::high_resolution_clock::now();
                fmt::print("The answer for day nine part two is {}\n", max + min);
                fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
                return;
            }
        }
    }
};
