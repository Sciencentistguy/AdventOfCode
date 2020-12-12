#pragma once

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <numeric>
#include <ranges>
#include <span>
#include <stack>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <af_vfs.h>
#include <ctre.hpp>
#include <fmt/color.h>
#include <fmt/core.h>

#include "common.h"
#include "computer.h"

struct day_eleven {
    enum class State { Empty, Occupied, Floor };
    using Seats = std::vector<State>;
    //    const Seats input;
    Seats input;
    int y_max;
    int x_max;

    day_eleven() {
        auto input_strings{readFile("Inputs/day_eleven.txt")};
        const auto start = std::chrono::high_resolution_clock::now();
        y_max = input_strings.size();
        x_max = input_strings[0].size();
        input.reserve(y_max * x_max);
        for (int y = 0; y < y_max; ++y) {
            for (int x = 0; x < x_max; ++x) {
                switch (input_strings[y][x]) {
                    case '.':
                        input.emplace_back(State::Floor);
                        break;
                    case 'L':
                        input.emplace_back(State::Empty);
                        break;
                    default:
                        throw std::runtime_error("Invalid input");
                }
            }
        }
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("Parsing input for day eleven took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    [[maybe_unused]] void printMap(const Seats& seats, int x_target,
                                   int y_target) const {  // prints the map, with the coordinates specified highlighted in red
        fmt::print("Map:\n");
        for (int y = 0; y < y_max; ++y) {
            for (int x = 0; x < x_max; ++x) {
                switch (seats[y * x_max + x]) {
                    case State::Floor:
                        if (x == x_target && y == y_target) {
                            fmt::print(fmt::fg(fmt::color::red), ".");
                        } else
                            fmt::print(".");
                        break;
                    case State::Empty:
                        if (x == x_target && y == y_target) {
                            fmt::print(fmt::fg(fmt::color::red), "L");
                        } else
                            fmt::print("L");
                        break;
                    case State::Occupied:
                        if (x == x_target && y == y_target) {
                            fmt::print(fmt::fg(fmt::color::red), "#");
                        } else
                            fmt::print("#");
                        break;
                }
            }
            fmt::print("\n");
        }
        fmt::print("\n");
    }

    [[maybe_unused]] void printMap(const Seats& seats) const {  // Prints the map
        fmt::print("Map:\n");
        for (int y = 0; y < y_max; ++y) {
            for (int x = 0; x < x_max; ++x) {
                switch (seats[y * x_max + x]) {
                    case State::Floor:
                        fmt::print(".");
                        break;
                    case State::Empty:
                        fmt::print("L");
                        break;
                    case State::Occupied:
                        fmt::print("#");
                        break;
                }
            }
            fmt::print("\n");
        }
        fmt::print("\n");
    }

    [[nodiscard]] inline int getAdjacents(const Seats& seats, int x, int y) const {
        int ret = 0;
        for (int i = y - 1; i <= y + 1; ++i) {
            for (int j = x - 1; j <= x + 1; ++j) {
                if (i == y && j == x)
                    continue;
                if (j >= 0 && i >= 0 && i < y_max && j < x_max)
                    ret += seats[i * x_max + j] == State::Occupied;
            }
        }
        return ret;
    }

    [[nodiscard]] inline int firstAdjacentSeats(const Seats& seats, const int x, const int y) const {
#define CURRENT_SEAT seats[i * x_max + j]
        int count = 0;
        //        int i = y;
        //        int j = x;

        // Directly up
        for (int i = y - 1, j = x; i >= 0; --i) {
            if (CURRENT_SEAT == State::Empty) {
                break;
            }
            if (CURRENT_SEAT == State::Occupied) {
                ++count;
                break;
            }
        }

        // Directly down
        for (int i = y + 1, j = x; i < y_max; ++i) {
            if (CURRENT_SEAT == State::Empty) {
                break;
            }
            if (CURRENT_SEAT == State::Occupied) {
                ++count;
                break;
            }
        }

        // Directly left
        for (int i = y, j = x - 1; j >= 0; --j) {
            if (CURRENT_SEAT == State::Empty) {
                break;
            }
            if (CURRENT_SEAT == State::Occupied) {
                ++count;
                break;
            }
        }

        // Directly right
        for (int i = y, j = x + 1; j < x_max; ++j) {
            if (CURRENT_SEAT == State::Empty) {
                break;
            }
            if (CURRENT_SEAT == State::Occupied) {
                ++count;
                break;
            }
        }

        // Up right
        for (int i = y - 1, j = x + 1; j < x_max && i >= 0; ++j, --i) {
            if (CURRENT_SEAT == State::Empty) {
                break;
            }
            if (CURRENT_SEAT == State::Occupied) {
                ++count;
                break;
            }
        }
        // Up left
        for (int i = y - 1, j = x - 1; j >= 0 && i >= 0; --j, --i) {
            if (CURRENT_SEAT == State::Empty) {
                break;
            }
            if (CURRENT_SEAT == State::Occupied) {
                ++count;
                break;
            }
        }
        // Down right
        for (int i = y + 1, j = x + 1; i < y_max && j < x_max; ++j, ++i) {
            if (CURRENT_SEAT == State::Empty) {
                break;
            }
            if (CURRENT_SEAT == State::Occupied) {
                ++count;
                break;
            }
        }
        // Down left
        for (int i = y + 1, j = x - 1; i < y_max && j >= 0; ++i, --j) {
            if (CURRENT_SEAT == State::Empty) {
                break;
            }
            if (CURRENT_SEAT == State::Occupied) {
                ++count;
                break;
            }
        }

        return count;
#undef CURRENT_SEAT
    }

    void part_one() const {
        const auto start = std::chrono::high_resolution_clock::now();
        bool change{true};
        auto seats = input;
        std::vector<int> adjacents(input.size(), 0);

        while (change) {
            change = false;
            for (int y = 0; y < y_max; ++y) {
                for (int x = 0; x < x_max; ++x) {
                    adjacents[y * x_max + x] = getAdjacents(seats, x, y);
                }
            }
            for (int y = 0; y < y_max; ++y) {
                for (int x = 0; x < x_max; ++x) {
                    const auto numAdjacents = adjacents[y * x_max + x];
                    auto& seat = seats[y * x_max + x];
                    if ((numAdjacents == 0) && (seat == State::Empty)) {
                        change = true;
                        seat = State::Occupied;
                        continue;
                    }
                    if ((numAdjacents >= 4) && (seat == State::Occupied)) {
                        change = true;
                        seat = State::Empty;
                        continue;
                    }
                }
            }
        }
        int count{0};
        for (const auto& seat : seats) {
            count += seat == State::Occupied;
        }

        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("The answer for day eleven part one is {}\n", count);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_two() const {
        const auto start = std::chrono::high_resolution_clock::now();

        bool change{true};
        auto seats = input;
        std::vector<int> adjacents(input.size(), 0);

        while (change) {
            change = false;
            for (int y = 0; y < y_max; ++y) {
                for (int x = 0; x < x_max; ++x) {
                    if (seats[y * x_max + x] == State::Floor) {
                        continue;
                    }
                    adjacents[y * x_max + x] = firstAdjacentSeats(seats, x, y);
                }
            }
            for (int y = 0; y < y_max; ++y) {
                for (int x = 0; x < x_max; ++x) {
                    const auto numAdjacents = adjacents[y * x_max + x];
                    auto& seat = seats[y * x_max + x];
                    if ((numAdjacents == 0) && (seat == State::Empty)) {
                        change = true;
                        seat = State::Occupied;
                        continue;
                    }
                    if ((numAdjacents >= 5) && (seat == State::Occupied)) {
                        change = true;
                        seat = State::Empty;
                        continue;
                    }
                }
            }
        }
        int count{0};
        for (const auto& seat : seats) {
            count += seat == State::Occupied;
        }

        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("The answer for day eleven part two is {}\n", count);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }
};
