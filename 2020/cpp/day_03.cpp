#include "day_03.h"

#include <ctre.hpp>

day_03::day_03() : input {readFile("Inputs/day_03.txt")} {
    fmt::print("Parsing input for day three took 0ns\n");
}

bool day_03::isTree(unsigned int x, unsigned int y) const {
    const auto width = input[0].length();
    return input[y][x % width] == '#';
}

unsigned int
day_03::getTreesSlope(unsigned int x_step, unsigned int y_step) const {
    unsigned int x {0};
    unsigned int y {0};
    unsigned int count {0};
    const auto y_limit = input.size();
    while (y < y_limit) {
        count += isTree(x, y);
        x += x_step;
        y += y_step;
    }
    return count;
}

void day_03::part_one() const {
    const auto start = std::chrono::high_resolution_clock::now();
    const auto count = getTreesSlope(3u, 1u);
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day three part one is {}\n", count);
    fmt::print(
        "Took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}

void day_03::part_two() const {
    const auto start = std::chrono::high_resolution_clock::now();
    long count = getTreesSlope(1u, 1u);
    count *= getTreesSlope(3u, 1u);
    count *= getTreesSlope(5u, 1u);
    count *= getTreesSlope(7u, 1u);
    count *= getTreesSlope(1u, 2u);
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day three part two is {}\n", count);
    fmt::print(
        "Took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}
