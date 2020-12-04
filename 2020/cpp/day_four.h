#pragma once

#include <cctype>
#include <chrono>
#include <cstdlib>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include <ctre.hpp>
#include <fmt/core.h>

#include "common.h"

struct day_four {
    using passport_t = std::unordered_map<std::string_view, std::string_view>;
    std::vector<std::string> input_strings;
    std::vector<passport_t> input;

    day_four() {
        input_strings = readFile("Inputs/day_four.txt");
        const auto start = std::chrono::high_resolution_clock::now();
        auto current_line = std::begin(input_strings);
        while (current_line != std::end(input_strings)) {
            passport_t& passport = input.emplace_back();
            if (*current_line == "") {
                ++current_line;
            }
            while (*current_line != "") {
                auto words = split(*current_line, ' ');
                for (const auto& sv : words) {
                    auto t = split(sv, ':');
                    // if (t.size() != 2) {
                    // throw std::runtime_error("Invalid input");
                    //}
                    passport.emplace(t[0], t[1]);
                }
                ++current_line;
            }
        }
        const auto end = std::chrono::high_resolution_clock::now();
        fmt::print("Parsing input for day four took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_one() const {
        const auto start = std::chrono::high_resolution_clock::now();
        int count{0};
        for (const auto& passport : input) {
            const bool valid = passport.contains("byr") && passport.contains("iyr") && passport.contains("eyr") && passport.contains("hgt")
                               && passport.contains("hcl") && passport.contains("ecl") && passport.contains("pid");
            count += valid;
        }
        const auto end = std::chrono::high_resolution_clock::now();

        fmt::print("The answer for day four part one is {}\n", count);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_two() const {
        const auto start = std::chrono::high_resolution_clock::now();
        int count{0};
        for (const auto& passport : input) {
            if (!(passport.contains("byr") && passport.contains("iyr") && passport.contains("eyr") && passport.contains("hgt") && passport.contains("hcl")
                  && passport.contains("ecl") && passport.contains("pid"))) {
                continue;
            }
            const auto byr = std::atoi(passport.at("byr").data());
            if (byr < 1920 || byr > 2002) {
                continue;
            }

            const auto iyr = std::atoi(passport.at("iyr").data());
            if (iyr < 2010 || iyr > 2020) {
                continue;
            }

            const auto eyr = std::atoi(passport.at("eyr").data());
            if (eyr < 2020 || eyr > 2030) {
                continue;
            }

            const auto hgt = passport.at("hgt");
            if (hgt.ends_with("in")) {
                const auto hgt = std::atoi(passport.at("hgt").data());
                if (hgt < 59 || hgt > 76) {
                    continue;
                }
            } else if (hgt.ends_with("cm")) {
                const auto hgt = std::atoi(passport.at("hgt").data());
                if (hgt < 150 || hgt > 193) {
                    continue;
                }
            } else {
                continue;
            }

            if (const auto m = ctre::match<"#[0123456789abcdefABCDEF]{6}">(passport.at("hcl")); !m) {
                continue;
            }

            const auto ecl = passport.at("ecl");
            if (!(ecl == "amb" || ecl == "blu" || ecl == "brn" || ecl == "gry" || ecl == "grn" || ecl == "hzl" || ecl == "oth")) {
                continue;
            }

            if (const auto m = ctre::match<"\\d{9}">(passport.at("pid")); !m) {
                continue;
            }

            ++count;
        }

        const auto end = std::chrono::high_resolution_clock::now();

        fmt::print("The answer for day four part two is {}\n", count);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }
};
