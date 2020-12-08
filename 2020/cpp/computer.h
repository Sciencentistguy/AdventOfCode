#pragma once
#include <tsl/robin_set.h>

enum class Instruction { Nop, Jmp, Acc };

struct instruction_t {
    Instruction instruction;
    int operand;
    bool operator==(const instruction_t& rhs) const {
        return instruction == rhs.instruction && operand == rhs.operand;
    }
    bool operator!=(const instruction_t& rhs) const {
        return !(rhs == *this);
    }
};

class computer_t {
    std::vector<instruction_t> code;
    int accumulator{0};
    std::vector<instruction_t>::iterator instruction_pointer;
    bool halted{false};
    std::vector<bool> seen;

 public:
    computer_t() = default;
    explicit computer_t(const std::vector<std::string>& input) {
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

    void nextInstruction() {
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

    void reset() {
        accumulator = 0;
        instruction_pointer = code.begin();
        seen = std::vector<bool>(code.size(), false);
    }

    [[nodiscard]] int getValueInAccumulator() const {
        return accumulator;
    }

    [[nodiscard]] bool seenNextInstruction() const {
        return seen[instruction_pointer - code.begin()];
    }

    [[nodiscard]] const std::vector<bool>& getSeen() const {
        return seen;
    }

    [[nodiscard]] std::vector<instruction_t>& getCode() {
        return code;
    }

    [[nodiscard]] bool isHalted() const {
        return halted;
    }
};