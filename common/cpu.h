#pragma once

#include <vector>
#include <string>

class CPU {
private:
    int haltFlag;
    int IOFlag;
    std::vector<int> outputs;
    int inputCounter;
    volatile long int pc;
    int relativeBase;


    void parse();

public:
    int getHaltFlag() const;

    std::vector<int> inputs;
    enum class IO_MODE {
        COMMAND_LINE = 0, VARIABLE
    };

    std::vector<long int> mem;

    std::vector<long int> run();

    void setInputs(const std::vector<int>& inputs);

    void setIoFlag(IO_MODE ioFlag);

    const std::vector<int>& getOutputs() const;

    explicit CPU(const std::string& program);

};
