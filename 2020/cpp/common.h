#pragma once

#include <fstream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

inline std::vector<std::string> readFile(const char* filename) {
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
    return out;
}

[[nodiscard]] inline std::vector<std::string> split(const std::string& s, char delimiter) {
    std::vector<std::string> tokens;
    std::string token;
    std::istringstream tokenStream(s);
    while (std::getline(tokenStream, token, delimiter)) {
        tokens.push_back(token);
    }
    return tokens;
}
