#include "day_08.h"

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
