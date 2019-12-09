class Node:
    def __init__(self, name):
        self.children = []
        self.parents = []
        self.name = name

    def add_parent(self, parent):
        self.parents.append(parent)

    def add_child(self, child):
        self.children.append(child)

    def get_set(self):
        return set(self.children + self.parents)

    def __repr__(self):
        return f"<Node: {self.name}>"


class Nodes:
    nodes = []

    def add_node(self, name, child):
        n = self.get_node(name)
        if n is None:
            n = Node(name)
            self.nodes.append(n)

        c = self.get_node(child)
        if c is None:
            c = Node(child)
            self.nodes.append(c)

        n.add_child(c)
        c.add_parent(n)

    def get_node(self, name) -> Node:
        for node in self.nodes:
            if node.name == name:
                return node
        return None


def search(nodes, start, aim):
    queue = [(start, [start])]
    while queue:
        node, path = queue.pop(0)
        for n in node.get_set() - set(path):
            if n == aim:
                yield path+[n]
            else:
                queue.append((n, path+[n]))


inpt = [i.rstrip().split(")") for i in open("input", "r").readlines()]

orbits = {x[1]: x[0] for x in inpt}

nodes = Nodes()

for k, v in orbits.items():
    nodes.add_node(k, v)
start = nodes.get_node("YOU")
aim = nodes.get_node("SAN")

path = list(next(search(nodes, start, aim)))
print(len(path)-3, path)
