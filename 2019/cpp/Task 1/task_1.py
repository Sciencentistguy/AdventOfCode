data = [int(i) for i in open("input", "r").readlines()]

proc = [int(i / 3)-2 for i in data]

out = sum(proc)

print(out)
