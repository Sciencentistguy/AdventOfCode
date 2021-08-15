#include "day_02.h"

#include <algorithm>

#include <ctre.hpp>
#include <range/v3/all.hpp>

day_02::line_t::line_t(
    int firstNum,
    int secondNum,
    char c,
    const std::string_view& str) :
    first_num {firstNum},
    second_num {secondNum},
    c {c},
    str {str} {}

day_02::day_02() : input_strings {readFile("Inputs/day_02.txt")} {
    const auto start = std::chrono::high_resolution_clock::now();
    for (const auto& str : input_strings) {
        auto m = ctre::match<R"(^(\d+)-(\d+) (\w): (\w+)$)">(str);

        const auto first_num = std::atoi(m.get<1>().to_view().data());
        const auto second_num = std::atoi(m.get<2>().to_view().data());
        const auto c = *(m.get<3>().to_view().data());
        auto s = m.get<4>().view();
        input.emplace_back(first_num, second_num, c, std::move(s));
    }
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print(
        "Parsing input for day two took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}

void day_02::part_one() const {
    const auto start = std::chrono::high_resolution_clock::now();
    int valid_passwords {0};
    for (const auto& line : input) {
        auto count =
            ranges::count_if(line.str, [&](char c) { return c == line.c; });
        valid_passwords +=
            (count >= line.first_num and count <= line.second_num);
    }
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day two part one is {}\n", valid_passwords);
    fmt::print(
        "Took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}

void day_02::part_two() const {
    const auto start = std::chrono::high_resolution_clock::now();
    int valid_passwords {0};
    for (const auto& line : input) {
        const auto first_num = line.first_num - 1;
        const auto second_num = line.second_num - 1;
        const auto pos1 = line.str[first_num];
        const auto pos2 = line.str[second_num];
        valid_passwords += (pos1 == line.c) != (pos2 == line.c);
    }
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day two part two is {}\n", valid_passwords);
    fmt::print(
        "Took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}
