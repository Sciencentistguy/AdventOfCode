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
    int pauseFlag;
    std::vector<int> inputs;
    std::vector<long int> mem;

    void parse();

public:
    int getHaltFlag() const;

    enum class IO_MODE {
        COMMAND_LINE = 0, VARIABLE
    };

    void addInput(const int& i);

    std::vector<long int>& getMem();

    bool isHalted() const;

    std::vector<long int> run();

    void setInputs(const std::vector<int>& inputs);

    void setIoFlag(IO_MODE ioFlag);

    const std::vector<int>& getOutputs() const;

    explicit CPU(const std::string& program);

};
