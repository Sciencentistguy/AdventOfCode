#pragma once

#include "common.h"

struct day_05 {
    struct boarding_pass_t {
        const int row;
        const int col;

        [[nodiscard]] inline int getSeatID() const;
    };

    std::vector<std::string> input_strings;
    std::vector<boarding_pass_t> input;

    day_05();

    void part_one() const;

    void part_two() const;
};
