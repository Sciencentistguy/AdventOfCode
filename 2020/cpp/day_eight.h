#pragma once

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <numeric>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <ctre.hpp>
#include <fmt/core.h>

#include "common.h"
#include "computer.h"

struct day_eight {
    std::vector<std::string> input_strings;
    computer_t computer;

    day_eight() : input_strings{readFile("Inputs/day_eight.txt")} {
        const auto start = std::chrono::high_resolution_clock::now();
        computer = computer_t(input_strings);
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("Parsing input for day eight took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_one() {
        const auto start = std::chrono::high_resolution_clock::now();
        while (true) {
            computer.nextInstruction();
            if (computer.seenNextInstruction()) {
                const auto end = std::chrono::high_resolution_clock::now();
                fmt::print("The answer for day eight part one is {}\n", computer.getValueInAccumulator());
                fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
                return;
            }
        }
    }

    void part_two() {
        const auto start = std::chrono::high_resolution_clock::now();
        size_t i{0};
        while (true) {
            computer.reset();

            auto& instruction_changed = computer.getCode()[i++];
            switch (instruction_changed.instruction) {
                case Instruction::Nop:
                    instruction_changed.instruction = Instruction::Jmp;
                    break;
                case Instruction::Jmp:
                    instruction_changed.instruction = Instruction::Nop;
                    break;
                case Instruction::Acc:
                    continue;
            }
            while (true) {
                computer.nextInstruction();
                if (computer.isHalted()) {
                    const auto end = std::chrono::high_resolution_clock::now();
                    fmt::print("The answer for day eight part one is {}\n", computer.getValueInAccumulator());
                    fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
                    return;
                }
                if (computer.seenNextInstruction()) {
                    switch (instruction_changed.instruction) {
                        case Instruction::Nop:
                            instruction_changed.instruction = Instruction::Jmp;
                            break;
                        case Instruction::Jmp:
                            instruction_changed.instruction = Instruction::Nop;
                            break;
                        case Instruction::Acc:
                            break;
                    }
                    break;
                }
            }
        }
    }
};
