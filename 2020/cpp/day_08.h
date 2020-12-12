#pragma once

#include "common.h"
#include "computer.h"

struct day_08 {
    std::vector<std::string> input_strings;
    computer_t computer;

    day_08();

    void part_one();

    void part_two();
};
