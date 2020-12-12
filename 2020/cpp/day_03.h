#pragma once

#include "common.h"

struct day_03 {
    const std::vector<std::string> input;

    day_03();

    inline bool isTree(unsigned int x, unsigned int y) const;

    inline unsigned int getTreesSlope(unsigned int x_step, unsigned int y_step) const;

    void part_one() const;

    void part_two() const;
};
