#include "day_12.h"

day_12::instruction_t::instruction_t(char opcode, ulong operand) :
    opcode {opcode},
    operand {operand} {}

day_12::day_12() :
    input {[] {
        const auto input_strings = readFile("Inputs/day_12.txt");
        const auto start = std::chrono::high_resolution_clock::now();
        std::vector<instruction_t> input;
        input.reserve(input_strings.size());
        for (const auto& line : input_strings) {
            input.emplace_back(
                *line.data(),
                static_cast<int>(fast_atol(line.data() + 1)));
        }
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print(
            "Parsing input for day twelve took {}ns\n",
            std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
                .count());
        return input;
    }()} {}

void day_12::part_one() const {
    const auto start = std::chrono::high_resolution_clock::now();
    int x {0};
    int y {0};
    int direction {1};
    for (auto [opcode, operand] : input) {
        switch (opcode) {
            case 'R':
                direction += operand / 90;
                break;
            case 'L':
                direction -= operand / 90;
                break;
            case 'F':
                switch (direction % 4) {
                    case 0:  // north
                        y += operand;
                        break;
                    case 1:  // east
                        x += operand;
                        break;
                    case 2:  // south
                        y -= operand;
                        break;
                    case 3:  // west
                        x -= operand;
                        break;
                }
                break;
            case 'N':
                y += operand;
                break;
            case 'E':
                x += operand;
                break;
            case 'S':
                y -= operand;
                break;
            case 'W':
                x -= operand;
                break;
        }
    }
    const auto distance = manhattan_distance(x, y, 0, 0);
    auto end = std::chrono::high_resolution_clock::now();

    fmt::print("The answer for day twelve part one is {}\n", distance);
    fmt::print(
        "Took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}

void day_12::part_two() const {
    const auto start = std::chrono::high_resolution_clock::now();
    int x {0};
    int y {0};
    int waypoint_x {10};
    int waypoint_y {1};
    for (auto [opcode, operand] : input) {
        switch (opcode) {
            case 'N':
                waypoint_y += operand;
                break;
            case 'E':
                waypoint_x += operand;
                break;
            case 'S':
                waypoint_y -= operand;
                break;
            case 'W':
                waypoint_x -= operand;
                break;
            case 'L':
                for (int i = 0; i < static_cast<int>(operand / 90); ++i) {
                    auto tmp = waypoint_x;
                    waypoint_x = -waypoint_y;
                    waypoint_y = tmp;
                }
                break;
            case 'R':
                for (int i = 0; i < static_cast<int>(operand / 90); ++i) {
                    auto tmp = waypoint_y;
                    waypoint_y = -waypoint_x;
                    waypoint_x = tmp;
                }
                break;
            case 'F':
                y += waypoint_y * operand;
                x += waypoint_x * operand;
                break;
        }
    }

    const auto distance = manhattan_distance(x, y, 0, 0);

    auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day twelve part two is {}\n", distance);
    fmt::print(
        "Took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}
