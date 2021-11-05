cmake_minimum_required(VERSION 3.10)

project(AdventOfCode2020)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(fmt)
find_package(ctre)

add_executable(AdventOfCode2020 main.cpp day_01.cpp day_02.cpp day_03.cpp day_04.cpp day_05.cpp day_07.cpp day_08.cpp day_09.cpp day_10.cpp day_06.cpp day_11.cpp day_12.cpp day_12.h day_13.cpp day_13.h day_14.cpp day_14.h day_15.cpp day_15.h day_16.cpp day_16.h day_17.cpp day_17.h)

target_link_libraries(AdventOfCode2020 fmt::fmt)
target_link_libraries(AdventOfCode2020 ctre::ctre)

install(TARGETS AdventOfCode2020 DESTINATION bin)
