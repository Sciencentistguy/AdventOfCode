inpt = [i.rstrip().split(")") for i in open("input", "r").readlines()]

orbits = {x[1]: x[0] for x in inpt}

c = 0
for k, v in orbits.items():
    c += 1
    while v != "COM":
        v = orbits.get(v, False)
        c += 1

print(c)
