#include <iostream>
#include <cstring>
#include <fstream>
#include <vector>
#include <sstream>

std::string getInput() {
    std::string str{};
    std::ifstream infile{"input"};

    if (!infile) {
        exit(1);
    }

    while (infile) {
        std::string str2{};
        infile >> str2;
        str += str2;
    }

    return str;
}

int main(int argc, char* argv[]) {
    std::vector<std::string> inputVector;
    std::vector<uint32_t> memory;
    std::string input = getInput();
    std::istringstream ss(input);
    std::string token;

    while (std::getline(ss, token, ',')) {
        inputVector.push_back(token);
    }

    for (auto s : inputVector) {
        memory.push_back(std::stoi(s));
    }

    memory[1] = 12;
    memory[2] = 02;

    for (int pos = 0; memory[pos] != 99; pos += 4) {
        switch ((memory[pos])) {
            case 1:
                memory[memory[pos + 3]] = memory[memory[pos + 1]] + memory[memory[pos + 2]];
                break;

            case 2:
                memory[memory[pos + 3]] = memory[memory[pos + 1]] * memory[memory[pos + 2]];
                break;
        }
    }

    std::cout << memory[0] << std::endl;
}
