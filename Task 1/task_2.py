import math

input_lines = [int(i.rstrip()) for i in open("input", "r").readlines()]

fuel = 0


def calc_fuel(i: int):
    return math.floor(i/3)-2


for module in input_lines:
    fuel_for_module = calc_fuel(module)

    while fuel_for_module > 0:
        fuel += fuel_for_module
        fuel_for_module = calc_fuel(fuel_for_module)

print(fuel)
