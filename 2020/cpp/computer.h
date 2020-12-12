#pragma once

#include <string>
#include <vector>

enum class Instruction { Nop, Jmp, Acc };

struct instruction_t {
    Instruction instruction;
    int operand;
    bool operator==(const instruction_t& rhs) const;
    bool operator!=(const instruction_t& rhs) const;
};

class computer_t {
    std::vector<instruction_t> code;
    int accumulator{0};
    std::vector<instruction_t>::iterator instruction_pointer;
    bool halted{false};
    std::vector<bool> seen;

 public:
    computer_t() = default;
    explicit computer_t(const std::vector<std::string>& input);

    void nextInstruction();

    void reset();

    [[nodiscard]] int getValueInAccumulator() const;

    [[nodiscard]] bool seenNextInstruction() const;

    [[nodiscard]] const std::vector<bool>& getSeen() const;

    [[nodiscard]] std::vector<instruction_t>& getCode();

    [[nodiscard]] bool isHalted() const;
};