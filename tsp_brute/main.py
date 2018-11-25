import random
import matplotlib.pyplot as plt
import math
import os

from tsp_brute import graph


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

def draw_shortest_path(size: int, n_vertices: int):
    g = create_cartesian_graph(size=size, n_vertices=n_vertices)
    vertices = g.vertices
    for x, y in vertices:
        plt.scatter(x, y)
    path = g.shortest_santa_path()
    plt.plot([p[0] for p in path], [p[1] for p in path])
    for i, p in enumerate(path):
        plt.text(p[0], p[1], str(i + 1))
    plt.xlim(left=0, right=size+1)
    plt.ylim(bottom=0, top=size+1)

def main():
    size = 5
    n_vertices = 8
    n_charts = 100

    for i in range(n_charts):
        print(f'{i+1}/{n_charts}')
        draw_shortest_path(size, n_vertices)
        plt.savefig(os.path.join('plots', f'path{size}@{n_vertices}_{i}.png'))
        plt.clf()

if __name__ == '__main__':
    main()
