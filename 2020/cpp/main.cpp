#include "day_one.h"
#include "day_three.h"
#include "day_two.h"
#include "day_four.h"

int main(int argc, char* argv[]) {
    day_one dayOne{};
    dayOne.part_one();
    dayOne.part_two();
    fmt::print("\n");

    day_two dayTwo{};
    dayTwo.part_one();
    dayTwo.part_two();
    fmt::print("\n");

    day_three dayThree{};
    dayThree.part_one();
    dayThree.part_two();
    fmt::print("\n");

    day_four dayFour{};
    dayFour.part_one();
    dayFour.part_two();
}
