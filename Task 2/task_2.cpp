#include <iostream>
#include <cstring>
#include <fstream>
#include <vector>
#include <sstream>
#include "cpu.h"

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
    for (int i = 0; i < 100; i++) {
        for (int j = 0; j < 100; j++) {
            CPU cpu{getInput()};
            cpu.mem[1] = i;
            cpu.mem[2] = j;
            auto mem = cpu.run();

            if (mem[0] == 19690720)
                printf("%02d%02d\n", mem[1], mem[2]);
        }
    }
}
