#pragma once

#include "common.h"

struct day_13 {
    int_fast32_t starting_time;
    std::vector<int_fast32_t> meaningful_bus_routes;
    std::vector<int_fast32_t> all_bus_routes;
    day_13();
    void part_one() const;
    void part_two() const;
};
