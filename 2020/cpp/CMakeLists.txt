cmake_minimum_required(VERSION 3.10)

project(AdventOfCode2020)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(fmt)
find_package(ctre)

add_executable(AdventOfCode2020 main.cpp computer.h day_01.cpp day_02.cpp day_03.cpp day_04.cpp day_05.cpp day_07.cpp day_08.cpp computer.cpp day_09.cpp day_10.cpp day_06.cpp day_11.cpp day_12.cpp day_12.h)

target_link_libraries(AdventOfCode2020 fmt::fmt)
target_link_libraries(AdventOfCode2020 ctre::ctre)
