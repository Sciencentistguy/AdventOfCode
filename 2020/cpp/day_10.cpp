#include "day_10.h"

#include <algorithm>
#include <cmath>

#include <range/v3/all.hpp>

day_10::day_10() :
    input {[] {
        auto input_strings {readFile("Inputs/day_10.txt")};
        const auto start = std::chrono::high_resolution_clock::now();
        std::vector<int> input;
        input.reserve(input_strings.size());
        for (const auto& line : input_strings) {
            input.push_back(fast_atol(line.c_str()));
        }
        ranges::sort(input);
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print(
            "Parsing input for day ten took {}ns\n",
            std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
                .count());
        return input;
    }()} {}

void day_10::part_one() {
    const auto start = std::chrono::high_resolution_clock::now();
    int count1Diff = 0;
    int count3Diff = 1;
    for (size_t i = 0; i < input.size(); ++i) {
        const auto diff = input[i + 1] - input[i];
        count1Diff += diff == 1;
        count3Diff += diff == 3;
    }
    count1Diff += input.front() == 1;
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print(
        "The answer for day ten part one is {}\n",
        count1Diff * count3Diff);
    fmt::print(
        "Took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}

void day_10::part_two() {
    const auto start = std::chrono::high_resolution_clock::now();
    std::vector<int> part2_input {0};
    ranges::copy(input, std::back_inserter(part2_input));
    part2_input.push_back(part2_input.back() + 3);

    struct interval_t {
        int64_t begin;
        int64_t end;
        uint64_t width;

        interval_t(int64_t begin, int64_t end, uint64_t width) :
            begin {begin},
            end {end},
            width {width} {}
    };

    size_t i = 0;
    size_t j = 0;

    std::vector<interval_t> intervals;
    intervals.reserve(part2_input.size());
    while (i < part2_input.size() && j <= part2_input.size()) {
        j = i + 1;
        while (j < part2_input.size() && part2_input[j] - part2_input[i] <= 3) {
            ++j;
        }
        if (j - i > 2) {
            const auto width = j - i - 2;
            if (intervals.empty()) {
                intervals.emplace_back(
                    part2_input[i],
                    part2_input[j - 1],
                    width);
            } else {
                const auto& interval = intervals.back();
                if (part2_input[i] < interval.end) {  // an intersection
                    intervals.back() = {
                        interval.begin,
                        part2_input[j - 1],
                        interval.width + width - 1};
                } else {
                    intervals.emplace_back(
                        part2_input[i],
                        part2_input[j - 1],
                        width);
                }
            }
        }
        ++i;
    }

    uint64_t count = 1;
    for (const auto& interval : intervals) {
        count *=
            std::pow(2, interval.width) - (interval.end - interval.begin > 3);
    }

    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day ten part two is {}\n", count);
    fmt::print(
        "Took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}
