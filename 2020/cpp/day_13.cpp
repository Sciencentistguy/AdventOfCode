#include "day_13.h"

day_13::day_13() {
    auto input_strings = readFile("Inputs/day_13.txt");
    const auto start = std::chrono::high_resolution_clock::now();
    starting_time = fast_atol(input_strings[0].data());
    auto splitted = split(input_strings[1], ',');
    for (const auto sv : splitted) {
        if (sv[0] == 'x') {
            all_bus_routes.push_back(-1);
            continue;
        }
        const auto val = fast_atol(sv.data());
        meaningful_bus_routes.push_back(val);
        all_bus_routes.push_back(val);
    }
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("Parsing input for day thirteen took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}

void day_13::part_one() const {
    const auto start = std::chrono::high_resolution_clock::now();
    auto i = starting_time;
    while (true) {
        for (auto route : meaningful_bus_routes) {
            if (i % route == 0) {
                const auto wait_time = i - starting_time;
                auto end = std::chrono::high_resolution_clock::now();
                fmt::print("The answer for day thirteen part one is {}\n", route * wait_time);
                fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
                return;
            }
        }
        ++i;
    }
}

void day_13::part_two() const {
    const auto start = std::chrono::high_resolution_clock::now();
    std::vector<std::pair<uint64_t, uint64_t>> wait_times;
    int minute{-1};
    uint64_t multiplier{0};
    for (const auto& bus : all_bus_routes) {
        ++minute;
        if (bus == -1) {
            continue;
        }
        if (minute == 0) {
            multiplier = bus;
        } else {
            wait_times.emplace_back(bus, bus - (minute % bus));
        }
    }
    uint64_t timestamp = 0;

    for (auto [first, second] : wait_times) {
        while ((timestamp % first) != second) {
            timestamp += multiplier;
        }
        multiplier *= first;
    }
    auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day thirteen part one is {}\n", timestamp);
    fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}
