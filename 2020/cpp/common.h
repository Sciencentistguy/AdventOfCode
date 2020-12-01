#pragma once

#include <fstream>
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
