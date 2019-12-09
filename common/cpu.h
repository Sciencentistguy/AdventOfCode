#pragma once

#include <vector>
#include <string>

class CPU {
private:
    int haltFlag;
    int IOFlag;
    std::vector<int> outputs;
    int inputCounter;
    volatile int pc;


    void parse();

public:
    int getHaltFlag() const;

    std::vector<int> inputs;
    enum class IO_MODE {
        COMMAND_LINE = 0, VARIABLE = 1
    };

    std::vector<int> mem;

    std::vector<int> run();

    void setInputs(const std::vector<int>& inputs);

    void setIoFlag(IO_MODE ioFlag);

    const std::vector<int>& getOutputs() const;

    explicit CPU(const std::string& program);

};
