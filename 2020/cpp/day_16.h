#pragma once
#include <robin_hood.h>

#include "common.h"

struct day_16 {
    struct range_t {
        uint64_t low1;
        uint64_t high1;
        uint64_t low2;
        uint64_t high2;

        [[nodiscard]] bool contains(uint64_t i) const;
    };

    const std::vector<std::string> input_strings;
    robin_hood::unordered_map<std::string_view, range_t> fields;
    std::vector<int> your_ticket;
    std::vector<std::vector<int>> nearby_tickets;

    day_16();

    [[nodiscard]] bool isTicketValid(const std::vector<int>& ticket) const;
    [[nodiscard]] bool isValueValidAnywhere(int field) const;


    void part_one() const;
    void part_two() const;
};