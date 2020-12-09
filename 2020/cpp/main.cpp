#include "day_eight.h"
#include "day_five.h"
#include "day_four.h"
#include "day_nine.h"
#include "day_one.h"
#include "day_seven.h"
#include "day_six.h"
#include "day_three.h"
#include "day_two.h"

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
    fmt::print("\n");

    day_five dayFive{};
    dayFive.part_one();
    dayFive.part_two();
    fmt::print("\n");

    day_six daySix{};
    daySix.part_one();
    daySix.part_two();
    fmt::print("\n");

    day_seven daySeven{};
    daySeven.part_one();
    daySeven.part_two();
    fmt::print("\n");

    day_eight dayEight{};
    dayEight.part_one();
    dayEight.part_two();
    fmt::print("\n");

    day_nine dayNine{};
    dayNine.part_one();
    dayNine.part_two();
}
