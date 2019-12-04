r = [int(i) for i in open("input", "r").read().rstrip().split(sep="-")]

p1 = p2 = 0
for n in range(r[0], r[1]):
    ns = str(n)
    repeats = [ns.count(d) for d in set(ns)]
    if ns == ''.join(sorted(ns)) and max(repeats) >= 2:
        p1 += 1
        if 2 in repeats:
            p2 += 1

print(f"Part 1: {p1}. Part 2: {p2}")
