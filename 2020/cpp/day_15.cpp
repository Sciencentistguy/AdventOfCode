#include "day_15.h"

#include <robin_hood.h>

day_15::day_15() :
    input {[] {
        auto input_strings {readFile("Inputs/day_15.txt")};
        const auto start = std::chrono::high_resolution_clock::now();
        std::vector<int> input;
        input.reserve(input_strings.size());
        for (const auto& line : input_strings) {
            for (auto num : split(line, ',')) {
                input.push_back(fast_atol(num.data()));
            }
        }
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print(
            "Parsing input for day fifteen took {}ns\n",
            std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
                .count());
        return input;
    }()} {}

void day_15::part_one() const {
    const auto start = std::chrono::high_resolution_clock::now();
    const auto result = do_task(2020);
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day fifteen part one is {}\n", result);
    fmt::print(
        "Took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}

void day_15::part_two() const {
    const auto start = std::chrono::high_resolution_clock::now();
    const auto result = do_task(30000000);
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day fifteen part two is {}\n", result);
    fmt::print(
        "Took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}

int day_15::do_task(int limit) const {
    auto vec = input;
    robin_hood::unordered_map<int, int> previous_occurence {};
    for (size_t i = 0; i < vec.size(); ++i) {
        previous_occurence[vec[i]] = i;
    }
    previous_occurence.erase(vec.back());
    int index = input.size();
    for (; index < limit; ++index) {
        const auto num = vec.back();
        if (previous_occurence.find(num) == previous_occurence.end()) {
            vec.push_back(0);
            previous_occurence[num] = index - 1;
        } else {
            const auto new_num = index - previous_occurence[num] - 1;
            vec.push_back(new_num);
            previous_occurence[num] = index - 1;
        }
    }
    return vec[limit - 1];
}
