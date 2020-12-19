#pragma once

#include <chrono>
#include <list>
#include <numeric>
#include <ranges>
#include <set>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

#include <ctre.hpp>
#include <fmt/core.h>

#include <robin_hood.h>
#include "common.h"

struct day_07 {
    struct rule_t {
        std::string_view colour;
        int number;
    };

    std::vector<std::string> input_strings;
    robin_hood::unordered_map<std::string_view, std::vector<rule_t>> rules;
    std::vector<std::string_view> bags;

    day_07();

    void part_one();

    void part_two();
};
