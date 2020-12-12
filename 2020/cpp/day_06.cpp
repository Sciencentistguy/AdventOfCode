#include "day_06.h"

#include <numeric>

int day_06::group_t::anyYes() const {
    std::array<bool, 26> isThere{};
    for (auto c : responses) {
        isThere[c - 'a'] = true;
    }
    return std::accumulate(std::begin(isThere), std::end(isThere), 0);
}

int day_06::group_t::allYes() const {
    std::array<int, 26> isThere{};
    for (auto c : responses) {
        ++isThere[c - 'a'];
    }
    int count{0};
    for (int i : isThere)
        count += i == number_of_responses;
    return count;
}

day_06::day_06() : input_strings{readFile("Inputs/day_06.txt")} {
    const auto start = std::chrono::high_resolution_clock::now();
    for (auto current_line = input_strings.begin(); current_line != input_strings.end();) {
        auto& group = input.emplace_back();
        if (*current_line == "")
            ++current_line;
        while (*current_line != "") {
            group.responses += *current_line;
            ++group.number_of_responses;
            ++current_line;
        }
    }
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("Parsing input for day six took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}

void day_06::part_one() const {
    const auto start = std::chrono::high_resolution_clock::now();

    long count{0};
    for (const auto& group : input) {
        count += group.anyYes();
    }

    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day six part one is {}\n", count);
    fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}

void day_06::part_two() const {
    const auto start = std::chrono::high_resolution_clock::now();

    long count{0};
    for (const auto& group : input) {
        count += group.allYes();
    }

    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day six part two is {}\n", count);
    fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}
