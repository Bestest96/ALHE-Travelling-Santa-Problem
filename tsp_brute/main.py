from tsp_brute import graph
import random
import matplotlib.pyplot as plt
import math


def cartesian_dist(x1, y1, x2, y2):
    return math.sqrt((x1-x2)**2 + (y1-y2)**2)

def create_cartesian_graph(*, size: int, n_vertices: int) -> graph.Graph:
    g = graph.Graph()
    points = []
    for n in random.sample(range(0, size**2), n_vertices):
        y = n//size + 1
        x = n%size + 1
        points.append((x, y,))

    for x1, y1 in points:
        for x2, y2 in points:
            if (x1, y1) == (x2, y2): continue
            g.add_edge((x1, y1,), (x2, y2,), cost=cartesian_dist(x1, y1, x2, y2))

    return g

def main():
    g = create_cartesian_graph(size=10, n_vertices=9)
    vertices = g.vertices
    for x, y in vertices:
        plt.scatter(x, y)
    path = g.shortest_santa_path()
    plt.plot([p[0] for p in path], [p[1] for p in path])
    for i, p in enumerate(path):
        plt.text(p[0], p[1], str(i+1))
    plt.show()

if __name__ == '__main__':
    for i in range(10):
        main()
