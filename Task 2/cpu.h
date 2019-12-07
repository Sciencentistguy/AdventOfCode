#pragma once

#include <vector>
#include <string>

class CPU {
    private:
        int flag;
        int inputRegister;
        volatile int pc;

        void parse();

    public:
        std::vector<int> mem;
        std::vector<int> run();

        CPU(const std::string& program);

};
