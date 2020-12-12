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

/**
 * @warning this function does not work for bases > 10
 */
inline constexpr unsigned long long int fast_atol(const char* buf, size_t len = 0, int base = 10) {
    if (len == 0) {
        len = std::strlen(buf);
    }
    unsigned int n = 0;
    while (len--) {
        if (!std::isdigit(*buf)) {
            return n;
        }
        n = (n * base) + (*buf++ - '0');
    }
    return n;
}

template<typename T>
inline void pop_front(std::vector<T>& vector) {
    vector.front() = std::move(vector.back());
    vector.pop_back();
}

struct hash_pair {
    template<class T1, class T2>
    size_t operator()(const std::pair<T1, T2>& p) const {
        const auto hash1 = std::hash<T1>{}(p.first);
        const auto hash2 = std::hash<T2>{}(p.second);
        using t = decltype(std::hash<T1>{}(p.first) + std::hash<T2>{}(p.second));
        return std::hash<t>{}(hash1 + hash2);
    }
};
