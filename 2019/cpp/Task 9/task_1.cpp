#include <fstream>
#include <iostream>
#include <algorithm>
#include "../common/cpu.h"

std::string getInput() {
    std::string str{};
    std::ifstream infile{"../Task 9/input"};

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
    CPU computer{getInput()};

    computer.setIoFlag(CPU::IO_MODE::COMMAND_LINE);

    computer.run();


}
