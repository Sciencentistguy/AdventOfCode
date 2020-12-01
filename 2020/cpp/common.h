#pragma once

#include <fstream>
#include <string>
#include <string_view>
#include <vector>

inline std::vector<std::string> readFile(const char* filename) {
    std::ifstream inputFile{};
    inputFile.open(filename);

    std::vector<std::string> out;
    std::string buffer;

    while (std::getline(inputFile, buffer)) {
        out.push_back(buffer);
    }
    return out;
}
