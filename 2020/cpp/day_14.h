#pragma once

#include <robin_hood.h>

#include "common.h"

struct day_14 {
    struct computer_t {
        struct mask_t {
            uint64_t ones;
            uint64_t zeros;
            uint64_t xs;
        };
        struct instruction_t {
            enum type_t { Mem, Mask };
            type_t type;
            uint64_t value{0};
            uint64_t address{0};
            mask_t mask{};
        };

        robin_hood::unordered_map<uint64_t, uint64_t> memory{};
        const std::vector<instruction_t>& input;
        std::vector<instruction_t>::const_iterator instruction_pointer;
        mask_t mask{};

        computer_t(const std::vector<instruction_t>& input);
        bool nextInstruction();
        void reset();
    };
    computer_t computer;
    const std::vector<computer_t::instruction_t> input;
    day_14();
    void part_one();
    void part_two() const;
};
