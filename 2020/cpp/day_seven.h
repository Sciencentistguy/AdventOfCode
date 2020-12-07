#pragma once

#include <chrono>
#include <list>
#include <numeric>
#include <ranges>
#include <set>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

#include <ctre.hpp>
#include <fmt/core.h>

#include "common.h"

struct day_seven {
    struct rule_t {
        std::string_view colour;
        int number;
    };
    std::vector<std::string> input_strings;
    std::unordered_map<std::string_view, std::vector<rule_t>> rules;
    std::vector<std::string_view> bags;

    day_seven() : input_strings{readFile("Inputs/day_seven.txt")} {
        const auto start = std::chrono::high_resolution_clock::now();
        for (const auto& line : input_strings) {
            auto words = split(line, ' ');
            const std::string_view colour = {&line.front(), &(words[1].back()) + 1};
            rules[colour] = {};
            std::span span{words.begin() + 4, words.end()};
            for (auto i = span.begin(); i < span.end(); i += 4) {
                auto& pair = rules[colour].emplace_back();
                pair.colour = {&i[1].front(), &(i[2].back()) + 1};
                pair.number = fast_atol(i[0].begin(), 1);
                fmt::print("");
            }
            bags.emplace_back(colour);
        }
        std::ranges::sort(bags);
        bags.erase(std::unique(bags.begin(), bags.end()), bags.end());

        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("Parsing input for day six took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_one() {
        const auto start = std::chrono::high_resolution_clock::now();
        int count{0};
        for (auto colour : bags) {
            if (colour == "shiny gold")
                continue;
            std::vector<std::string_view> candidates;
            candidates.reserve(rules.size());
            candidates.push_back(colour);
            while (!candidates.empty()) {
                auto bag = candidates.front();
                if (bag == "shiny gold") {
                    ++count;
                    break;
                }
                for (const auto& subbag : rules[bag]) {
                    candidates.emplace_back(subbag.colour);
                }
                pop_front(candidates);
            }
        }
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("The answer for day seven part one is {}\n", count);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_two() {
        const auto start = std::chrono::high_resolution_clock::now();
        int count{0};
        std::vector<rule_t> candidates;
        candidates.push_back({"shiny gold", 1});
        while (!candidates.empty()) {
            const auto& bag = candidates.front();
            count += bag.number;

            for (const auto& subbag : rules[bag.colour]) {
                candidates.emplace_back(subbag.colour, subbag.number* bag.number);
            }
            pop_front(candidates);
        }
        --count;  // we need to remove the original shiny gold bag
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("The answer for day seven part two is {}\n", count);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }
};
