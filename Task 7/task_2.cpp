#include <fstream>
#include <iostream>
#include <algorithm>
#include "../common/cpu.h"

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

void findPermutations(int a[], std::vector<std::vector<int>>& vec) {

    // Sort the given array
    std::sort(a, a + 5);

    // Find all possible permutations
    do {
        vec.push_back({a[0], a[1], a[2], a[3], a[4]});
    } while (std::next_permutation(a, a + 5));
}


int main(int argc, char* argv[]) {
    auto input = getInput();

    std::vector<std::vector<int>> vec;

    int arr[5] = {5, 6, 7, 8, 9};

    findPermutations(arr, vec);

    int max = 0;

    for (auto a : vec) {
        CPU amp1 = CPU(input);
        CPU amp2 = CPU(input);
        CPU amp3 = CPU(input);
        CPU amp4 = CPU(input);
        CPU amp5 = CPU(input);
        amp1.setIoFlag(CPU::IO_MODE::VARIABLE);
        amp2.setIoFlag(CPU::IO_MODE::VARIABLE);
        amp3.setIoFlag(CPU::IO_MODE::VARIABLE);
        amp4.setIoFlag(CPU::IO_MODE::VARIABLE);
        amp5.setIoFlag(CPU::IO_MODE::VARIABLE);
        amp1.addInput(a[0]);
        amp2.addInput(a[1]);
        amp3.addInput(a[2]);
        amp4.addInput(a[3]);
        amp5.addInput(a[4]);
        amp1.addInput(0);
        while (!amp5.isHalted()) {
            amp1.run();
            amp2.addInput(amp1.getOutputs().back());
            amp2.run();
            amp3.addInput(amp2.getOutputs().back());
            amp3.run();
            amp4.addInput(amp3.getOutputs().back());
            amp4.run();
            amp5.addInput(amp4.getOutputs().back());
            amp5.run();
            amp1.addInput(amp5.getOutputs().back());
        }

        if (amp5.getOutputs().back() > max) {
            max = amp5.getOutputs().back();
        }
    }

    std::cout << max << std::endl;
}
