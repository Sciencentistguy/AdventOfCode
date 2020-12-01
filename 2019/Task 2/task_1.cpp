#include <iostream>
#include <cstring>
#include <fstream>
#include <vector>
#include <sstream>
#include "../common/cpu.h"

std::string getInput() {
    std::string str{};
    std::ifstream infile{"../Task 2/input"};

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
    CPU cpu{getInput()};
    cpu.getMem()[1] = 12;
    cpu.getMem()[2] = 02;
    auto mem = cpu.run();
    std::cout << mem[0] << std::endl;
}
