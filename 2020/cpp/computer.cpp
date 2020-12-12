#include "computer.h"

#include <stdexcept>

bool instruction_t::operator==(const instruction_t& rhs) const {
    return instruction == rhs.instruction && operand == rhs.operand;
}

bool instruction_t::operator!=(const instruction_t& rhs) const {
    return !(rhs == *this);
}

computer_t::computer_t(const std::vector<std::string>& input) {
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

void computer_t::nextInstruction() {
    const auto& current_instruction = *instruction_pointer;

    seen[instruction_pointer - code.begin()] = true;

    if (instruction_pointer - code.begin() >= code.size()) {
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

void computer_t::reset() {
    accumulator = 0;
    instruction_pointer = code.begin();
    seen = std::vector<bool>(code.size(), false);
}

int computer_t::getValueInAccumulator() const {
    return accumulator;
}

bool computer_t::seenNextInstruction() const {
    return seen[instruction_pointer - code.begin()];
}

const std::vector<bool>& computer_t::getSeen() const {
    return seen;
}

std::vector<instruction_t>& computer_t::getCode() {
    return code;
}

bool computer_t::isHalted() const {
    return halted;
}
