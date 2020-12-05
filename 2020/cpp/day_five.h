#pragma once

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <numeric>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <ctre.hpp>
#include <fmt/core.h>

#include "common.h"

struct day_five {
    struct boarding_pass_t {
        const int row;
        const int col;
        //        const std::string_view rowPath;
        //        const std::string_view colPath;

        //        inline int getRow() const {
        //
        //            int upper_limit{127};
        //            int lower_limit{0};
        //            for (const auto c : rowPath) {
        //                switch (c) {
        //                    case 'F':
        //                        upper_limit = std::midpoint(upper_limit, lower_limit) - 1;
        //                        break;
        //                    case 'B':
        //                        lower_limit = std::midpoint(upper_limit, lower_limit);
        //                        break;
        //                }
        //            }
        //
        //            return lower_limit;
        //        }
        //
        //        inline int getCol() const {
        //            int upper_limit{7};
        //            int lower_limit{0};
        //            for (const auto c : colPath) {
        //                switch (c) {
        //                    case 'L':
        //                        upper_limit = std::midpoint(upper_limit, lower_limit) - 1;
        //                        break;
        //                    case 'R':
        //                        lower_limit = std::midpoint(upper_limit, lower_limit);
        //                        break;
        //                }
        //            }
        //            return lower_limit;
        //        }

        inline int getSeatID() const {
            return (row * 8) + col;
        }
    };
    std::vector<std::string> input_strings;
    std::vector<boarding_pass_t> input;

    day_five() : input_strings{readFile("Inputs/day_five.txt")}{
        const auto start = std::chrono::high_resolution_clock::now();
        for (auto& str : input_strings) {
            std::replace_if(
                std::begin(str), std::end(str), [](char c) { return c == 'F' || c == 'L'; }, '0');
            std::replace_if(
                std::begin(str), std::end(str), [](char c) { return c == 'B' || c == 'R'; }, '1');

            auto firstSeven = std::string_view(std::begin(str), std::begin(str) + 7);
            auto lastThree = std::string_view(std::end(str) - 3, std::end(str));
            const auto row = fast_atoi(firstSeven.data(), 7, 2);
            const auto col = fast_atoi(lastThree.data(), 3, 2);
            input.emplace_back(row, col);
        }
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("Parsing input for day five took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_one() const {
        const auto start = std::chrono::high_resolution_clock::now();
        int max{0};
        for (const auto& boarding_pass : input) {
            max = std::max(max, boarding_pass.getSeatID());
        }
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("The answer for day five part one is {}\n", max);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_two() const {
        const auto start = std::chrono::high_resolution_clock::now();
        auto rng = input | std::ranges::views::transform([](const boarding_pass_t& boarding_pass) { return boarding_pass.getSeatID(); });
        std::vector<int> ids{rng.begin(), rng.end()};

        std::ranges::sort(ids);

        const auto lowest = ids.front();
        const auto highest = ids.back();

        for (int i = lowest; i <= highest; ++i) {
            if (!std::ranges::binary_search(ids, i))
                if (std::ranges::binary_search(ids, i-1) && std::ranges::binary_search(ids, i+1)) {
                    const auto end = std::chrono::high_resolution_clock::now();
                    fmt::print("The answer for day five part two is {}\n", i);
                    fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
                    return;
                }
        }
    }
};
