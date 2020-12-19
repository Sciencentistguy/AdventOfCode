#pragma once
#include "common.h"

struct day_15 {
    const std::vector<int> input;

    day_15();
    [[nodiscard]] int do_task(int limit) const;

    void part_one() const;
    void part_two() const;

};
