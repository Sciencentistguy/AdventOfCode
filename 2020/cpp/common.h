#pragma once

#include <chrono>
#include <fstream>
#include <iterator>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#include <fmt/core.h>

inline std::vector<std::string> readFile(const char* filename) {
    const auto start = std::chrono::high_resolution_clock::now();
    std::ifstream inputFile{};
    inputFile.open(filename);
    if (!inputFile.is_open()) {
        throw std::runtime_error("File not found");
    }

    std::vector<std::string> out;
    std::string buffer;

    while (std::getline(inputFile, buffer)) {
        out.push_back(buffer);
    }
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("Reading file {} took {}ns\n", filename, std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count());
    return out;
}

inline std::vector<std::string_view> split(std::string_view str, char delimiter) {
    int lastSplit{0};
    std::vector<std::string_view> out;
    const auto begin = std::begin(str);
    for (int i = 0; i < str.length(); i++) {
        if (str[i] == delimiter) {
            out.emplace_back(begin + lastSplit, i - lastSplit);
            ++i;
            lastSplit = i;
        }
    }
    out.emplace_back(begin + lastSplit, begin + str.length());
    return out;
}

template<typename T, typename Iterator>
inline constexpr auto enumerate(T&& range) requires std::input_iterator<Iterator> {
    struct iterator {
        size_t i;
        Iterator iter;
        bool operator!=(const iterator& other) const {
            return iter != other.iter;
        }
        void operator++() {
            ++i;
            ++iter;
        }
        auto operator*() const {
            return std::tie(i, *iter);
        }
    };
    static_assert(std::input_iterator<iterator>);
    struct iterable_wrapper {
        T iterable;
        auto begin() {
            return iterator{0, std::begin(iterable)};
        }
        auto end() {
            return iterator{0, std::end(iterable)};
        }
    };
    return iterable_wrapper{std::forward<T>(range)};
}

inline constexpr unsigned int fast_atoi(const char* buf, size_t len, int base = 10) {
    unsigned int n = 0;
    while (len--) {
        n = (n * base) + (*buf++ - '0');
    }
    return n;
}
