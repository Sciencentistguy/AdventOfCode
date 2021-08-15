#pragma once

#include "common.h"

struct day_06 {
    struct group_t {
        int number_of_responses {};
        std::string responses;

        [[nodiscard]] int anyYes() const;

        [[nodiscard]] int allYes() const;
    };

    std::vector<std::string> input_strings;
    std::vector<group_t> input;

    day_06();

    void part_one() const;

    void part_two() const;
};
