#include "day_09.h"

#include <span>

day_09::day_09() {
    auto input_strings{readFile("Inputs/day_09.txt")};
    const auto start = std::chrono::high_resolution_clock::now();
    for (const auto& line : input_strings) {
        input.push_back(fast_atol(line.c_str()));
    }
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("Parsing input for day nine took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}

constexpr bool day_09::sumOfPairs(int64_t target, int64_t* begin, size_t length) {
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

constexpr const int64_t* day_09::sequenceOfNumbersSum(int64_t target, const int64_t* begin) {
    int64_t sum{0};
    while (sum < target) {
        sum += *begin++;
    }
    if (sum == target) {
        return begin;
    }
    return nullptr;
}

void day_09::part_one() {
    const auto start = std::chrono::high_resolution_clock::now();
    constexpr int offset{25};
    for (size_t i = offset; i < input.size(); ++i) {
        if (!sumOfPairs(input[i], &input[i - offset])) {
            const auto end = std::chrono::high_resolution_clock::now();
            fmt::print("The answer for day nine part one is {}\n", input[i]);
            fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
            part_one_answer = input[i];
            return;
        }
    }
}

void day_09::part_two() const {
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
