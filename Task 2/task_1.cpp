#include <iostream>
#include <cstring>
#include <fstream>
#include <vector>
#include <sstream>
#include "../Intcode/cpu.h"

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
    CPU cpu{getInput()};
    cpu.mem[1] = 12;
    cpu.mem[2] = 02;
    auto mem = cpu.run();
    std::cout << mem[0] << std::endl;
}
