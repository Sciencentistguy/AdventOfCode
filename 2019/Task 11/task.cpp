#include <fstream>
#include <iostream>
#include <algorithm>
#include <utility>
#include <map>
#include <set>
#include "../common/cpu.h"

std::string getInput() {
    std::string str{};
    std::ifstream infile{"../Task 11/input"};

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

class Painter {
public:

    enum class COLOUR {
        BLACK = 0, WHITE
    };

    enum class DIRECTION {
        UP, RIGHT, DOWN, LEFT
    };

    CPU cpu;
    std::map<std::pair<int, int>, COLOUR> page;
    std::map<std::pair<int, int>, bool> modified_page;
    int x;
    int y;
    DIRECTION direction;
    int count;
    std::set<std::pair<int, int>> painted;

    Painter(CPU cpu, int x, int y) : cpu(std::move(cpu)) {
        for (int i = 0; i < y; ++i) {
            for (int j = 0; j < x; ++j) {
                page[std::make_pair(j, i)] = COLOUR::BLACK;
                modified_page[std::make_pair(j, i)] = false;
            }
        }
        Painter::cpu.setIOMode(CPU::IO_MODE::VARIABLE);
        count = 0;
        Painter::x = x / 2;
        Painter::y = y / 2;
        direction = DIRECTION::UP;
    }

    void move_forward() {
        switch (direction) {
            case DIRECTION::UP:
                y++;
                break;
            case DIRECTION::RIGHT:
                x++;
                break;
            case DIRECTION::DOWN:
                y--;
                break;
            case DIRECTION::LEFT:
                x--;
                break;
        }
    }

    void turn_left() {
        switch (direction) {
            case DIRECTION::UP:
                direction = DIRECTION::LEFT;
                break;
            case DIRECTION::RIGHT:
                direction = DIRECTION::UP;
                break;
            case DIRECTION::DOWN:
                direction = DIRECTION::RIGHT;
                break;
            case DIRECTION::LEFT:
                direction = DIRECTION::DOWN;
                break;
        }
    }

    void turn_right() {
        switch (direction) {
            case DIRECTION::UP:
                direction = DIRECTION::RIGHT;
                break;
            case DIRECTION::RIGHT:
                direction = DIRECTION::DOWN;
                break;
            case DIRECTION::DOWN:
                direction = DIRECTION::LEFT;
                break;
            case DIRECTION::LEFT:
                direction = DIRECTION::UP;
                break;
        }
    }

    void run() {
        while (!cpu.isHalted()) {
            cpu.addInput(page[std::make_pair(x, y)] == COLOUR::BLACK ? 0 : 1);
            cpu.run();
            auto location = std::make_pair(x,y);
            if (page[location] != (COLOUR) cpu.getOutputs().back()) {
                page[location] = (COLOUR) cpu.getOutputs().back();
                painted.emplace(location);
            }
            cpu.getOutputs().end()[-2] ? turn_right() : turn_left();
            move_forward();
        }
    }
};


int main(int argc, char* argv[]) {
    std::string input = getInput();
    Painter p{CPU(input), 100, 100};
    p.run();
    std::cout << p.painted.size() << std::endl;
}
