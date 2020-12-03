#pragma once

#include <chrono>
#include <cstdlib>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <ctre.hpp>
#include <fmt/core.h>

#include "common.h"

struct day_two {
    struct line_t {
        const int first_num;
        const int second_num;
        const char c;
        const std::string str;
    };
    std::vector<line_t> input;

    static bool homogenous(std::string_view sv) {
        return sv.find_first_not_of(sv[0]) == std::string_view::npos;
    }

    day_two() {
        const auto input_strings = readFile("Inputs/day_two.txt");
        const auto start = std::chrono::high_resolution_clock::now();
        for (const auto& str : input_strings) {
            auto m = ctre::match<R"(^(\d+)-(\d+) (\w): (\w+)$)">(str);
            // if (!m)
            // throw std::runtime_error("Invalid input");

            const auto start = std::atoi(m.get<1>().to_view().data());
            const auto end = std::atoi(m.get<2>().to_view().data());
            const auto c = *(m.get<3>().to_view().data());
            auto s = m.get<4>().to_string();
            input.emplace_back(start, end, c, std::move(s));
        }
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("Parsing input for day two took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_one() const {
        const auto start = std::chrono::high_resolution_clock::now();
        int valid_passwords{0};
        for (const auto& line : input) {
            auto count = std::count_if(std::begin(line.str), std::end(line.str), [&](char c) { return c == line.c; });
            valid_passwords += (count >= line.first_num and count <= line.second_num);
        }
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("The answer for day two part one is {}\n", valid_passwords);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_two() const {
        const auto start = std::chrono::high_resolution_clock::now();
        int valid_passwords{0};
        for (const auto& line : input) {
            // if (line.second_num > line.str.size())
            // throw std::runtime_error("Invalid input");

            const auto first_num = line.first_num - 1;
            const auto second_num = line.second_num - 1;
            const auto pos1 = line.str[first_num];
            const auto pos2 = line.str[second_num];
            valid_passwords += (pos1 == line.c) != (pos2 == line.c);
        }
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("The answer for day two part two is {}\n", valid_passwords);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }
};
