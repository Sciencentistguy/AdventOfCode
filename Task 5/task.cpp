#include <fstream>
#include <iostream>
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
    CPU cpu = CPU(getInput());
    auto mem = cpu.run();
}
