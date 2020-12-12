#include "day_05.h"

#include <algorithm>

int day_05::boarding_pass_t::getSeatID() const {
    return (row * 8) + col;
}

day_05::day_05() : input_strings{readFile("Inputs/day_05.txt")} {
    const auto start = std::chrono::high_resolution_clock::now();
    for (auto& str : input_strings) {
        std::ranges::replace_if(
            str, [](char c) { return c == 'F' || c == 'L'; }, '0');
        std::ranges::replace_if(
            str, [](char c) { return c == 'B' || c == 'R'; }, '1');

        auto firstSeven = std::string_view(std::begin(str), std::begin(str) + 7);
        auto lastThree = std::string_view(std::end(str) - 3, std::end(str));
        const auto row = fast_atol(firstSeven.data(), 7, 2);
        const auto col = fast_atol(lastThree.data(), 3, 2);
        input.emplace_back(row, col);
    }
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("Parsing input for day five took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}

void day_05::part_one() const {
    const auto start = std::chrono::high_resolution_clock::now();
    int max{0};
    for (const auto& boarding_pass : input) {
        max = std::max(max, boarding_pass.getSeatID());
    }
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day five part one is {}\n", max);
    fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}

void day_05::part_two() const {
    const auto start = std::chrono::high_resolution_clock::now();
    auto rng = std::ranges::views::transform(input, [](const boarding_pass_t& boarding_pass) { return boarding_pass.getSeatID(); });
    std::vector<int> ids{rng.begin(), rng.end()};

    std::ranges::sort(ids);

    const auto lowest = ids.front();
    const auto highest = ids.back();

    for (int i = lowest; i <= highest; ++i) {
        if (!std::ranges::binary_search(ids, i))
            if (std::ranges::binary_search(ids, i - 1) && std::ranges::binary_search(ids, i + 1)) {
                const auto end = std::chrono::high_resolution_clock::now();
                fmt::print("The answer for day five part two is {}\n", i);
                fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
                return;
            }
    }
}
