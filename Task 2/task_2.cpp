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

    int out;
    std::vector<uint32_t> aux{memory};

    for (int pos1 = 0; pos1 < 100; pos1++) {
        for (int pos2 = 0; pos2 < 100; pos2++) {
            memory = aux;
            memory[1] = pos1;
            memory[2] = pos2;

            for (int currPos = 0; memory[currPos] != 99; currPos += 4) {
                switch ((memory[currPos])) {
                    case 1:
                        memory[memory[currPos + 3]] = memory[memory[currPos + 1]] + memory[memory[currPos + 2]];
                        break;

                    case 2:
                        memory[memory[currPos + 3]] = memory[memory[currPos + 1]] * memory[memory[currPos + 2]];
                        break;
                }
            }

            if (memory[0] == 19690720) {
                std::cout << 100 * pos1 + pos2 << std::endl;
                pos1 = 100;
                pos2 = 100;
            }
        }
    }

    std::cout << out << std::endl;
}
