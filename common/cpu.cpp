#include <vector>
#include <sstream>
#include <iostream>
#include "cpu.h"


CPU::CPU(const std::string& program) {
    setIOMode(IO_MODE::COMMAND_LINE);
    relativeBase = 0;
    pauseFlag = 0;
    haltFlag = 0;
    inputCounter = 0;
    pc = 0;
    inputs = {};
    outputs = {};


    std::istringstream ss(program);
    std::string token;
    std::vector<std::string> inputVector;

    while (std::getline(ss, token, ',')) {
        inputVector.push_back(token);
    }

    for (const auto& s : inputVector) {
        mem.push_back(std::stol(s));
    }

    for (int i = 0; i < 0xffff; ++i) {
        mem.push_back(0);
    }

}

void CPU::parse() {
    if (mem[pc] == 99) {
        haltFlag = 1;
        return;
    }

    int instruction = (mem[pc]) % 100;
    int DataModeA = (mem[pc] / 100) % 10;
    int DataModeB = (mem[pc] / 1000) % 10;
    int DataModeC = (mem[pc] / 10000) % 10;

    long int* parA = nullptr;
    long int* parB = nullptr;
    long int* parC = nullptr;

    if (DataModeA == 0)
        parA = &mem[mem[pc + 1]];
    else if (DataModeA == 1)
        parA = &mem[pc + 1];
    else if (DataModeA == 2)
        parA = &mem[mem[(pc + 1)] + relativeBase];


    if (DataModeB == 0)
        parB = &mem[mem[pc + 2]];
    else if (DataModeB == 1)
        parB = &mem[pc + 2];
    else if (DataModeB == 2)
        parB = &mem[mem[(pc + 2)] + relativeBase];


    if (DataModeC == 0)
        parC = &mem[mem[pc + 3]];
    else if (DataModeC == 1)
        parC = &mem[pc + 3];
    else if (DataModeC == 2)
        parC = &mem[mem[(pc + 3)] + relativeBase];


    switch (instruction) {
        case 1:
            *parC = *parA + *parB;
            pc += 4;
            break;

        case 2:
            *parC = *parA * *parB;
            pc += 4;
            break;

        case 3:
            if (!IOFlag) {
                std::cout << "Input: ";
                std::cin >> *parA;
            } else {
                if (inputCounter >= inputs.size()) {
                    pauseFlag = 1;
                    break;
                } else {
                    *parA = inputs[inputCounter++];
                }
            }
            pc += 2;
            break;

        case 4:
            if (!IOFlag) {
                std::cout << *parA << std::endl;
            } else {
                outputs.push_back(*parA);
            }
            pc += 2;
            break;

        case 5:
            pc = *parA ? *parB : pc + 3;
            break;

        case 6:
            pc = !*parA ? *parB : pc + 3;
            break;

        case 7:
            *parC = *parA < *parB;
            pc += 4;
            break;

        case 8:
            *parC = *parA == *parB;
            pc += 4;
            break;

        case 9:
            relativeBase += (int) *parA;
            pc += 2;
            break;

        default:
            std::cout << "You broke it\n";
            exit(1);
    }
}

std::vector<long int> CPU::run() {
    pauseFlag = 0;
    while (haltFlag != 1 && pauseFlag != 1) {
        parse();
    }

    return mem;
}

const std::vector<int>& CPU::getOutputs() const {
    return outputs;
}

void CPU::setInputs(const std::vector<int>& inputs) {
    CPU::inputs = inputs;
}

void CPU::setIOMode(IO_MODE ioFlag) {
    IOFlag = static_cast<int>(ioFlag);
}

int CPU::getHaltFlag() const {
    return haltFlag;
}

bool CPU::isHalted() const {
    return (bool) haltFlag;
}

void CPU::addInput(const int& i) {
    inputs.push_back(i);
}

std::vector<long int>& CPU::getMem() {
    return mem;
}

