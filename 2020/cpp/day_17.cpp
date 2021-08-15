#include "day_17.h"

#include <range/v3/all.hpp>

day_17::day_17() {
    auto input_strings {readFile("Inputs/day_17.txt")};
    const auto start = std::chrono::high_resolution_clock::now();
    glm::ivec3 max_xy3 {};
    for (const auto& line : input_strings) {
        max_xy3.x = 0;
        for (char c : line) {
            grid3[max_xy3] = c == '#';
            ++max_xy3.x;
        }
        ++max_xy3.y;
    }
    expand_grid(grid3, max_xy3);

    glm::ivec4 max_xy4 {};
    for (const auto& line : input_strings) {
        max_xy4.x = 0;
        for (char c : line) {
            grid4[max_xy4] = c == '#';
            ++max_xy4.x;
        }
        ++max_xy4.y;
    }
    expand_grid(grid4, max_xy4);

    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print(
        "Parsing input for day seventeen took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}

template<typename T>
void day_17::expand_grid(grid_t<T>& grid, T max_xy) {
    for (int z = -1; z <= 1; ++z) {
        for (int y = -1; y <= max_xy.y; ++y) {
            for (int x = -1; x <= max_xy.x; ++x) {
                if constexpr (max_xy.length() == 4) {
                    for (int tmp_w = -1; tmp_w <= 1; ++tmp_w) {
                        glm::ivec4 pos = {x, y, z, tmp_w};
                        if (!grid.contains(pos)) {
                            grid[pos] = false;
                        }
                    }
                } else {
                    glm::ivec3 pos = {x, y, z};
                    if (!grid.contains(pos)) {
                        grid[pos] = false;
                    }
                }
            }
        }
    }
}

template<typename T>
int day_17::get_active_adjacents(grid_t<T>& grid, T centre) {
    int count = 0;

    for (int z = centre.z - 1; z <= centre.z + 1; ++z) {
        for (int y = centre.y - 1; y <= centre.y + 1; ++y) {
            for (int x = centre.x - 1; x <= centre.x + 1; ++x) {
                if constexpr (centre.length() == 4) {
                    for (int w = centre.w - 1; w <= centre.w + 1; ++w) {
                        if (centre != glm::ivec4 {x, y, z, w}) {
                            count += grid[glm::ivec4 {x, y, z, w}];
                        }
                    }
                } else {
                    if (centre != glm::ivec3 {x, y, z}) {
                        count += grid[glm::ivec3 {x, y, z}];
                    }
                }
            }
        }
    }
    return count;
}

template<typename T>
void day_17::do_conway(grid_t<T>& grid) {
    auto working_copy = grid;
    for (auto& [location, active] : grid) {
        const auto adjacents = get_active_adjacents(working_copy, location);
        if (active && adjacents != 2 && adjacents != 3) {
            active = false;
        } else if (adjacents == 3) {
            active = true;
        }
    }
    grid.merge(working_copy);
}

void day_17::part_one() {
    const auto start = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < 6; ++i) {
        do_conway(grid3);
    }
    const auto result = ranges::count(grid3 | ranges::views::values, true);
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day seventeen part one is {}\n", result);
    fmt::print(
        "Took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}

void day_17::part_two() {
    const auto start = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < 6; ++i) {
        do_conway(grid4);
    }
    const auto result = ranges::count(grid4 | ranges::views::values, true);
    const auto end = std::chrono::high_resolution_clock::now();
    fmt::print("The answer for day seventeen part two is {}\n", result);
    fmt::print(
        "Took {}ns\n",
        std::chrono::duration_cast<std::chrono::nanoseconds>(end - start)
            .count());
}
