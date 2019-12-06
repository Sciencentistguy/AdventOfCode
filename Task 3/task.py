def distance(pos1: tuple, pos2: tuple) -> int:
    return abs(pos2[0]-pos1[0]) + abs(pos2[1]-pos1[1])


def do_wire(wire):
    ret = {}
    x = y = 0
    count = 0
    for instruction in wire:
        direction = instruction[0]
        length = int(instruction[1:])

        if direction == "R":
            def operation(x, y): return (x+1, y)
        if direction == "L":
            def operation(x, y): return (x-1, y)
        if direction == "U":
            def operation(x, y): return (x, y+1)
        if direction == "D":
            def operation(x, y): return (x, y-1)

        for i in range(length):
            x, y = operation(x, y)
            count += 1
            ret[(x, y)] = count
    return ret


inpt = [x.split(sep=",") for x in open("input", "r").readlines()]

wire1 = do_wire(inpt[0])
wire2 = do_wire(inpt[1])

collisions = set(wire1.keys()) & set(wire2.keys())
print(f"Part 1: {min(distance(x,(0,0)) for x in collisions)}")

distances = [wire1[i] + wire2[i] for i in collisions]
print(f"Part 2: {min(distances)}")
