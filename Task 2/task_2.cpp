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
    for (int i = 0; i < 100; i++) {
        for (int j = 0; j < 100; j++) {
            CPU cpu{getInput()};
            cpu.getMem()[1] = i;
            cpu.getMem()[2] = j;
            auto mem = cpu.run();

            if (mem[0] == 19690720) {
                printf("%02ld%02ld\n", mem[1], mem[2]);
                return 0;
            }
        }
    }
}
