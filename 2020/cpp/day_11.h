#pragma once

#include "common.h"

struct day_11 {
    enum class State { Empty, Occupied, Floor };

    using Seats = std::vector<State>;
    Seats input;
    int y_max;
    int x_max;

    day_11();

    [[maybe_unused]] void
    printMap(const Seats& seats, int x_target, int y_target) const;

    [[maybe_unused]] void printMap(const Seats& seats) const;

    [[nodiscard]] inline int
    getAdjacents(const Seats& seats, int x, int y) const;

    [[nodiscard]] inline int
    firstAdjacentSeats(const Seats& seats, const int x, const int y) const;

    void part_one() const;

    void part_two() const;
};
