#include "day_16.h"

#include <iostream>

#include <ctre.hpp>
#include <range/v3/all.hpp>

bool day_16::range_t::contains(int i) const {
    const auto in1 = (i >= low1 && i <= high1);
    const auto in2 = (i >= low2 && i <= high2);

    return in1 || in2;
}

day_16::day_16() : input_strings{readFile("Inputs/day_16.txt")} {
    const auto start = std::chrono::high_resolution_clock::now();
    auto sections = split(input_strings, std::string(""));

    for (const auto& line : sections[0]) {
        auto m = ctre::match<R"(^([\w ]+): (\d+)-(\d+) or (\d+)-(\d+))">(line);
        fields[m.get<1>().to_view()] = {fast_atol(m.get<2>().to_view().data()), fast_atol(m.get<3>().to_view().data()),
                                        fast_atol(m.get<4>().to_view().data()), fast_atol(m.get<5>().to_view().data())};
    }

    for (const auto num : split(sections[1][1], ',')) {
        your_ticket.push_back(fast_atol(num.data()));
    }

    for (int i = 1; i < sections[2].size(); ++i) {
        auto& ticket = nearby_tickets.emplace_back();
        for (const auto num : split(sections[2][i], ',')) {
            ticket.push_back(fast_atol(num.data()));
        }
    }
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("Parsing input for day sixteen took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}

bool day_16::isTicketValid(const std::vector<int>& ticket) const {
    auto rng = ranges::views::filter(ticket, [this](auto field) { return isValueValidAnywhere(field); });
    return std::distance(std::begin(rng), std::end(rng)) == ticket.size();
}

bool day_16::isValueValidAnywhere(int field) const {
    int failed{0};
    for (const auto [field_name, field_range] : fields) {
        if (!field_range.contains(field)) {
            ++failed;
        }
    }
    return failed != fields.size();
}

void day_16::part_one() const {
    const auto start = std::chrono::high_resolution_clock::now();
    auto count{0};
    for (const auto& ticket : nearby_tickets) {
        for (auto field : ticket) {
            if (!isValueValidAnywhere(field)) {
                count += field;
            }
        }
    }
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day sixteen part one is {}\n", count);
    fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}

void day_16::part_two() const {
    const auto start = std::chrono::high_resolution_clock::now();
    const auto valid_tickets =
        ranges::views::filter(nearby_tickets, [this](const std::vector<int>& ticket) { return isTicketValid(ticket); }) | ranges::to_vector;
    std::vector<std::pair<std::vector<int>, std::string_view>> candidates;
    for (const auto [field_name, field_range] : fields) {
        std::vector<int> possible;
        for (int i = 0; i < your_ticket.size(); ++i) {
            bool passes = true;
            for (const auto& ticket : valid_tickets) {
                if (!field_range.contains(ticket[i])) {
                    passes = false;
                    break;
                }
            }
            if (passes) {
                possible.push_back(i);
            }
        }
        candidates.emplace_back(possible, field_name);
    }

    std::ranges::sort(candidates, [](auto a, auto b) { return a.first.size() < b.first.size(); });

    robin_hood::unordered_map<std::string_view, int> field_and_index;
    std::vector<bool> taken(your_ticket.size(), false);
    for (const auto& candidate : candidates) {
        for (int index : candidate.first) {
            if (!taken[index]) {
                field_and_index[candidate.second] = index;
                taken[index] = true;
            }
        }
    }
    int64_t result{1};
    for (const auto [field, index] : field_and_index) {
        if (field.find("departure") != std::string_view::npos) {
            result *= your_ticket[index];
        }
    }

    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day sixteen part two is {}\n", result);
    fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
}
