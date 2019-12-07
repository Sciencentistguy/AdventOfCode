#include <vector>
#include <sstream>
#include <iostream>
#include "cpu.h"


CPU::CPU(const std::string& program) {
    std::istringstream ss(program);
    std::string token;
    std::vector<std::string> inputVector;

    while (std::getline(ss, token, ',')) {
        inputVector.push_back(token);
    }

    for (const auto& s : inputVector) {
        mem.push_back(std::stoi(s));
    }
}

void CPU::parse() {
    if (mem[pc] == 99) {
        std::cout << "halt()" << std::endl;
        flag = 1;
        return;
    }

    int instruction = (mem[pc]) % 100;
    int DataModeA = (mem[pc] / 100) % 10;
    int DataModeB = (mem[pc] / 1000) % 10;
    int DataModeC = (mem[pc] / 10000) % 10;
    int& parA = DataModeA ? mem[pc + 1] : mem[mem[pc + 1]];
    int& parB = DataModeB ? mem[pc + 2] : mem[mem[pc + 2]];
    int& parC = DataModeC ? mem[pc + 3] : mem[mem[pc + 3]];

    switch (instruction) {
        case 1:
            std::cerr << "add(" << parC << ", " << parA << ", " << parB << ")" << std::endl;
            parC = parA + parB;
            pc += 4;
            break;

        case 2:
            std::cerr << "multiply(" << parC << ", " << parA << ", " << parB << ")" << std::endl;
            parC = parA * parB;
            pc += 4;
            break;

        case 3:
            std::cerr << "input(" << parA << ")" << std::endl;
            std::cout << "Input: ";
            std::cin >> parA;
            pc += 2;
            break;

        case 4:
            std::cerr << "output(" << parA << ")" << std::endl;
            std::cout << parA << std::endl;
            pc += 2;
            break;

        case 5:
            std::cerr << "jump_if_true(" << parA << ", " << parB << ")" << std::endl;
            pc = parA ? parB : pc + 3;
            break;

        case 6:
            std::cerr << "jump_if_false(" << parA << ", " << parB << ")" << std::endl;
            pc = !parA ? parB : pc + 3;
            break;

        case 7:
            std::cerr << "less_than(" << parC << ", " << parA << ", " << parB << ")" << std::endl;
            parC = parA < parB;
            pc += 4;
            break;

        case 8:
            std::cerr << "more_than(" << parC << ", " << parA << ", " << parB << ")" << std::endl;
            parC = parA == parB;
            pc += 4;
            break;

        default:
            std::cout << "You broke it\n";
            exit(1);
    }
}

std::vector<int> CPU::run() {
    pc = 0;
    flag = 0;

    while (flag != 1) {
        parse();
    }

    return mem;
}
