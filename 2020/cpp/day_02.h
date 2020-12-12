#pragma once

#include "common.h"

struct day_02 {
    struct line_t {
        const int first_num;
        const int second_num;
        const char c;
        const std::string_view str;
    };

    std::vector<std::string> input_strings;
    std::vector<line_t> input;

    day_02();

    void part_one() const;

    void part_two() const;
};
