#include "day_one.h"
#include "day_two.h"

int main(int argc, char* argv[]) {
    day_one dayOne{};
    dayOne.part_one();
    dayOne.part_two();

    fmt::print("\n");
    day_two dayTwo{};
    dayTwo.part_one();
    dayTwo.part_two();
}
