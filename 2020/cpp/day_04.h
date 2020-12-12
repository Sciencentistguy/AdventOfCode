#pragma once

#include "common.h"

struct day_04 {
    struct passport_t {
        int birth_year{};
        int issue_year{};
        int expiration_year{};
        std::string_view height_unit{};
        int height{};
        std::string_view hair_colour{};
        std::string_view eye_colour{};
        std::string_view passport_id{};

        [[nodiscard]] bool hasAllFields() const;
    };

    std::vector<std::string> input_strings;
    std::vector<passport_t> input;

    day_04();

    void part_one() const;

    void part_two() const;
};
