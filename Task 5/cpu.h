#pragma once

#include <vector>
#include <string>

class CPU {
private:
    std::vector<int> mem;
    int flag;
    int inputRegister;
    volatile int pc;

    void parse();

public:
    std::vector<int> run();

    CPU(const std::string& program);

};
