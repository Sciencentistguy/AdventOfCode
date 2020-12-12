#pragma once

#include "common.h"

struct day_01 {
    std::vector<int> input;
    static constexpr int target = 2020;

    day_01();

    void part_one() const;

    void part_two() const;
};
