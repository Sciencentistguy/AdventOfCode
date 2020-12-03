#pragma once

#include <chrono>
#include <cstdlib>
#include <exception>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <ctre.hpp>
#include <fmt/core.h>

#include "common.h"

struct day_three {
    std::vector<std::string> input;

    day_three() : input{readFile("Inputs/day_three.txt")} {
        fmt::print("Parsing input for day three took 0ns\n");
    }

    inline bool isTree(unsigned int x, unsigned int y) const {
        const auto width = input[0].length();
        return input[y][x % width] == '#';
    }

    inline unsigned int getTreesSlope(unsigned int x_step, unsigned int y_step) const {
        unsigned int x{};
        unsigned int y{};
        unsigned int count{};
        const auto y_limit = input.size() - y_step;
        do {
            count += isTree(x, y);
            x += x_step;
            y += y_step;
        } while (y <= y_limit);
        return count;
    }

    void part_one() const {
        auto start = std::chrono::high_resolution_clock::now();
        auto count = getTreesSlope(3u, 1u);
        auto end = std::chrono::high_resolution_clock::now();
        fmt::print("The answer for day three part one is {}\n", count);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_two() const {
        auto start = std::chrono::high_resolution_clock::now();
        long count = getTreesSlope(1u, 1u);
        count *= getTreesSlope(3u, 1u);
        count *= getTreesSlope(5u, 1u);
        count *= getTreesSlope(7u, 1u);
        count *= getTreesSlope(1u, 2u);
        auto end = std::chrono::high_resolution_clock::now();
        fmt::print("The answer for day three part two is {}\n", count);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }
};
