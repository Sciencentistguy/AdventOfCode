#pragma once

#define GLM_ENABLE_EXPERIMENTAL
#include <glm/gtx/hash.hpp>
#include <glm/vec3.hpp>
#include <glm/vec4.hpp>
#include <robin_hood.h>

#include "common.h"

struct day_17 {
    template<typename T>
    requires(requires { std::hash<T>(); }) using grid_t = std::unordered_map<T, bool>;  // for some reason robin_hood::unordered_map doesn't have a merge()

    grid_t<glm::ivec3> grid3;
    grid_t<glm::ivec4> grid4;

    day_17();

    template<typename T>
    static void expand_grid(grid_t<T>& grid, T max_xy);

    template<typename T>
    static int get_active_adjacents(grid_t<T>& grid, T centre);

    template<typename T>
    static void do_conway(grid_t<T>& grid);

    void part_one();
    void part_two();
};
