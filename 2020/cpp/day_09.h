#pragma once

#include "common.h"

struct day_09 {
    std::vector<int64_t> input;
    int64_t part_one_answer{0};

    day_09();

    static constexpr bool sumOfPairs(int64_t target, int64_t* begin, size_t length = 25);

    static constexpr const int64_t* sequenceOfNumbersSum(int64_t target, const int64_t* begin);

    void part_one();

    void part_two() const;
};
