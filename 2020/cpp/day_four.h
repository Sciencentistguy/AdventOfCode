#pragma once

#include <cctype>
#include <chrono>
#include <cstdlib>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <ctre.hpp>
#include <fmt/core.h>

#include "common.h"

struct day_four {
    struct passport_t {
        int birth_year{};
        int issue_year{};
        int expiration_year{};
        std::string_view height_unit{};
        int height{};
        std::string_view hair_colour{};
        std::string_view eye_colour{};
        std::string_view passport_id{};

        bool hasAllFields() const {
            return birth_year && issue_year && expiration_year && height && !height_unit.empty() && !hair_colour.empty() && !eye_colour.empty()
                   && !passport_id.empty();
        }
    };

    std::vector<std::string> input_strings;
    std::vector<passport_t> input;

    day_four() : input_strings{readFile("Inputs/day_four.txt")} {
        const auto start = std::chrono::high_resolution_clock::now();
        auto current_line = std::begin(input_strings);
        while (current_line != std::end(input_strings)) {
            passport_t& passport = input.emplace_back();
            if (*current_line == "") {
                ++current_line;
            }
            while (*current_line != "") {
                auto words = split(*current_line, ' ');
                for (const auto& pair : words) {
                    auto t = split(pair, ':');
                    if (t[0] == "byr") {
                        passport.birth_year = std::atoi(t[1].data());
                    } else if (t[0] == "iyr") {
                        passport.issue_year = std::atoi(t[1].data());
                    } else if (t[0] == "eyr") {
                        passport.expiration_year = std::atoi(t[1].data());
                    } else if (t[0] == "hgt") {
                        passport.height_unit = std::string_view{std::end(t[1]) - 2, 2};
                        passport.height = std::atoi(t[1].data());
                    } else if (t[0] == "hcl") {
                        passport.hair_colour = t[1];
                    } else if (t[0] == "ecl") {
                        passport.eye_colour = t[1];
                    } else if (t[0] == "pid") {
                        passport.passport_id = t[1];
                    } else if (t[0] == "cid") {
                        // We do not care
                    } else {
                        throw std::runtime_error("Invalid input");
                    }
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
            count += passport.hasAllFields();
        }
        const auto end = std::chrono::high_resolution_clock::now();

        fmt::print("The answer for day four part one is {}\n", count);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }

    void part_two() const {
        const auto start = std::chrono::high_resolution_clock::now();
        int count{0};
        for (const auto& passport : input) {
            if (!passport.hasAllFields()) {
                continue;
            }

            if (passport.birth_year < 1920 || passport.birth_year > 2002) {
                continue;
            }

            if (passport.issue_year < 2010 || passport.issue_year > 2020) {
                continue;
            }

            if (passport.expiration_year < 2020 || passport.expiration_year > 2030) {
                continue;
            }

            if (passport.height_unit == "in") {
                if (passport.height < 59 || passport.height > 76) {
                    continue;
                }
            } else if (passport.height_unit == "cm") {
                if (passport.height < 150 || passport.height > 193) {
                    continue;
                }
            } else {
                continue;
            }

            if (const auto m = ctre::match<"#[0123456789abcdefABCDEF]{6}">(passport.hair_colour); !m) {
                continue;
            }

            if (!(passport.eye_colour == "amb" || passport.eye_colour == "blu" || passport.eye_colour == "brn" || passport.eye_colour == "gry"
                  || passport.eye_colour == "grn" || passport.eye_colour == "hzl" || passport.eye_colour == "oth")) {
                continue;
            }

            if (const auto m = ctre::match<"\\d{9}">(passport.passport_id); !m) {
                continue;
            }

            ++count;
        }

        const auto end = std::chrono::high_resolution_clock::now();

        fmt::print("The answer for day four part two is {}\n", count);
        fmt::print("Took {}ns\n", std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    }
};
