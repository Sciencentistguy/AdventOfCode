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

    std::vector<std::vector<int>> vec;

    int arr[5] = {0, 1, 2, 3, 4};

    findPermutations(arr, vec);

    int max=0;

    for (auto a : vec) {
        CPU amp1 = CPU(getInput());
        CPU amp2 = CPU(getInput());
        CPU amp3 = CPU(getInput());
        CPU amp4 = CPU(getInput());
        CPU amp5 = CPU(getInput());
        amp1.setIoFlag(CPU::IO_MODE::VARIABLE);
        amp2.setIoFlag(CPU::IO_MODE::VARIABLE);
        amp3.setIoFlag(CPU::IO_MODE::VARIABLE);
        amp4.setIoFlag(CPU::IO_MODE::VARIABLE);
        amp5.setIoFlag(CPU::IO_MODE::VARIABLE);
        amp1.setInputs({a[0], 0});
        amp1.run();
        amp2.setInputs({a[1], amp1.getOutputs()[0]});
        amp2.run();
        amp3.setInputs({a[2], amp2.getOutputs()[0]});
        amp3.run();
        amp4.setInputs({a[3], amp3.getOutputs()[0]});
        amp4.run();
        amp5.setInputs({a[4], amp4.getOutputs()[0]});
        amp5.run();

        if (amp5.getOutputs()[0] > max) {
            max = amp5.getOutputs()[0];
        }
    }

    std::cout << max << std::endl;
}
