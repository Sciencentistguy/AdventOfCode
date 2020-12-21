#include "day_08.h"


bool day_08::instruction_t::operator==(const instruction_t& rhs) const {
    return instruction == rhs.instruction && operand == rhs.operand;
}

bool day_08::instruction_t::operator!=(const instruction_t& rhs) const {
    return !(rhs == *this);
}

day_08::computer_t::computer_t(const std::vector<std::string>& input) {
    code.reserve(input.size());
    for (const auto& line : input) {
        auto& instruction = code.emplace_back();
        switch (line[0]) {
            case 'n':
                instruction.instruction = Instruction::Nop;
                break;
            case 'a':
                instruction.instruction = Instruction::Acc;
                break;
            case 'j':
                instruction.instruction = Instruction::Jmp;
                break;
            default:
                throw std::runtime_error("Invalid input");
        }
        instruction.operand = std::atoi(line.c_str() + 4);
    }
    seen = std::vector<bool>(code.size(), false);
    instruction_pointer = code.begin();
}

void day_08::computer_t::nextInstruction() {
    const auto& current_instruction = *instruction_pointer;

    seen[instruction_pointer - code.begin()] = true;

    if (instruction_pointer - code.begin() >= static_cast<long>(code.size())) {
        halted = true;
        return;
    }

    switch (current_instruction.instruction) {
        case Instruction::Nop:
            ++instruction_pointer;
            return;
        case Instruction::Jmp:
            instruction_pointer += current_instruction.operand;
            return;
        case Instruction::Acc:
            accumulator += current_instruction.operand;
            ++instruction_pointer;
            return;
        default:
            throw std::runtime_error("Invalid code for computer");
    }
}

void day_08::computer_t::reset() {
    accumulator = 0;
    instruction_pointer = code.begin();
    seen = std::vector<bool>(code.size(), false);
}

int day_08::computer_t::getValueInAccumulator() const {
    return accumulator;
}

bool day_08::computer_t::seenNextInstruction() const {
    return seen[instruction_pointer - code.begin()];
}

const std::vector<bool>& day_08::computer_t::getSeen() const {
    return seen;
}

std::vector<day_08::instruction_t>& day_08::computer_t::getCode() {
    return code;
}

bool day_08::computer_t::isHalted() const {
    return halted;
}

day_08::day_08() : input_strings{readFile("Inputs/day_08.txt")} {
    const auto start = std::chrono::high_resolution_clock::now();
    computer = computer_t(input_strings);
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("Parsing input for day eight took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}

void day_08::part_one() {
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

void day_08::part_two() {
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
