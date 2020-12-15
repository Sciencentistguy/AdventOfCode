#include "day_14.h"

day_14::computer_t::computer_t(const std::vector<instruction_t>& input) : input{input}, instruction_pointer{input.begin()} {
}

bool day_14::computer_t::nextInstruction() {
    switch (instruction_pointer->type) {
        case instruction_t::Mask:
            mask = instruction_pointer->mask;
            break;
        case instruction_t::Mem:
            const auto zeroed = instruction_pointer->value & (~mask.zeros);
            const auto masked = zeroed | mask.ones;
            memory[instruction_pointer->address] = masked;
            break;
    }
    ++instruction_pointer;
    if (instruction_pointer == input.end()) {
        return false;
    }
    return true;
}
void day_14::computer_t::reset() {
    instruction_pointer = input.begin();
    memory.clear();
}

day_14::day_14() :
    input{[] {
        const auto input_strings = readFile("Inputs/day_14.txt");
        const auto start = std::chrono::high_resolution_clock::now();
        std::vector<computer_t::instruction_t> input;
        for (const auto& line : input_strings) {
            auto& instruction = input.emplace_back();
            switch (line[1]) {
                case 'a': {  // mask instruction
                    instruction.type = computer_t::instruction_t::Mask;
                    auto* const mask_str = line.data() + 7;
                    for (int i = 35; i >= 0; --i) {
                        switch (mask_str[i]) {
                            case '1':
                                instruction.mask.ones |= 1ull << (35 - i);
                                break;
                            case '0':
                                instruction.mask.zeros |= 1ull << (35 - i);
                                break;
                            case 'X':
                                instruction.mask.xs |= 1ull << (35 - i);
                                break;
                        }
                    }
                    // fmt::print("str:\t{}\nones:\t{:0>36b}\nzeroes:\t{:0>36b}\nXs:\t\t{:0>36b}\n", mask_str, instruction.mask.ones, instruction.mask.zeros,
                    //            instruction.mask.xs);
                    break;
                }
                case 'e':  // mem instruction
                    instruction.type = computer_t::instruction_t::Mem;
                    auto splitted = split(line, '=');
                    instruction.address = fast_atol(line.data() + 4);
                    instruction.value = fast_atol(splitted.back().data() + 1);
                    break;
            }
        }
        const auto end = std::chrono::high_resolution_clock::now();
      fmt::print("Parsing input for day fourteen took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
        return input;
    }()},
    computer{input} {
}

void day_14::part_one() {
    const auto start = std::chrono::high_resolution_clock::now();
    computer.reset();
    while (computer.nextInstruction()) {
    }
    uint64_t count{0};
    for (auto [addr, val] : computer.memory) {
        count += val;
    }
    auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day fourteen part one is {}\n", count);
    fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}

void day_14::part_two() const {
}
