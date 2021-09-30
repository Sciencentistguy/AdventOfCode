from collections import defaultdict
from math import ceil


def decode_symbol(c: str) -> tuple:
    units, name = c.split(" ")
    return int(units), name


def ore_required(chemical, needed, wasted=None):
    global formulae
    if wasted is None:
        wasted = defaultdict(int)
    if chemical == "ORE":
        return needed
    reuse = min(needed, wasted[chemical])
    needed -= reuse
    wasted[chemical] -= reuse
    produced, inputs = formulae[chemical]
    n = ceil(needed / produced)
    ret = 0
    for required, inpt in inputs:
        ret += ore_required(inpt, n * required, wasted)
    wasted[chemical] += n * produced - needed
    return ret


formulae = {decode_symbol(splitted[1])[1]: (decode_symbol(splitted[1])[0], [decode_symbol(chem) for chem in splitted[0].split(", ")]) for splitted in
            [line.strip().split(" => ") for line in open("input", "r").readlines()]}

print(f"Part 1: {ore_required('FUEL', 1)}")

max_ore = 1000000000000
upper_bound = 1
lower_bound = 0
x = 0
while ore_required("FUEL", upper_bound) < max_ore:
    lower_bound = upper_bound
    upper_bound *= 2

while lower_bound + 1 < upper_bound:
    midpoint = (lower_bound + upper_bound) // 2
    n = ore_required("FUEL", midpoint)
    if n > max_ore:
        upper_bound = midpoint
    if n < max_ore:
        lower_bound = midpoint

print(f"Part 2: {lower_bound}")

