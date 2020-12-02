cmake_minimum_required(VERSION 3.10)

project(AdventOfCode2020)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(fmt)
find_package(ctre)

add_executable(AdventOfCode2020 main.cpp)

target_link_libraries(AdventOfCode2020 fmt::fmt)
target_link_libraries(AdventOfCode2020 ctre::ctre)
