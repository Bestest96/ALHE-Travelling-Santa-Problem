from collections import namedtuple
import itertools
import math

from typing import List, Any, NoReturn, Set, Dict, Iterable


# we'll use infinity as a default distance to nodes.
inf = float('inf')
HalfEdge = namedtuple('HalfEdge', 'end, cost')


class Graph:
    def __init__(self, edges: List[Iterable]=None) -> NoReturn:
        self.neighbours = dict()
        if edges is None: return

        # let's check that the data is right
        wrong_edges = [edge for edge in edges if len(edge) not in [2, 3]]
        if wrong_edges:
            raise ValueError(f'Wrong edges data: {wrong_edges}')

        for edge in edges: self.add_edge(*edge)

    @property
    def vertices(self) -> List[Any]:
        return list(self.neighbours.keys())

    def get_node_pairs(self, n1, n2, both_ends=True):
        if both_ends:
            node_pairs = [[n1, n2], [n2, n1]]
        else:
            node_pairs = [[n1, n2]]
        return node_pairs

    def add_edge(self, n1: Any, n2: Any, cost: float=1, *, both_ends: bool=True) -> NoReturn:
        node_pairs = self.get_node_pairs(n1, n2, both_ends)
        for v1, v2 in node_pairs:
            if not v1 in self.neighbours: self.neighbours[v1] = dict()
            if v2 in self.neighbours[v1]:
                return ValueError(f'Edge {v1} {v2} already exists')

        self.neighbours[n1][n2] = cost
        if both_ends:
            self.neighbours[n2][n1] = cost

    def remove_edge(self, n1, n2, both_ends=False):
        node_pairs = self.get_node_pairs(n1, n2, both_ends)
        for v1, v2 in node_pairs:
            self.neighbours[v1].pop(v2)

    def dijkstra(self, source, dest):
        if not source in self.neighbours:
            raise ValueError('Unknown source node.')
        if not dest in self.neighbours:
            raise ValueError('Unknown destination node.')

        distances = {vertex: inf for vertex in self.vertices}
        previous_vertices = {
            vertex: None for vertex in self.vertices
        }
        distances[source] = 0
        vertices = self.vertices.copy()

        while vertices:
            current_vertex = min(vertices, key=lambda vertex: distances[vertex])
            vertices.remove(current_vertex)
            if distances[current_vertex] == inf: break
            for neighbour, cost in self.neighbours[current_vertex].items():
                alternative_route = distances[current_vertex] + cost
                if alternative_route < distances[neighbour]:
                    distances[neighbour] = alternative_route
                    previous_vertices[neighbour] = current_vertex

        path, current_vertex = [], dest
        while current_vertex is not None:
            path.append(current_vertex)
            current_vertex = previous_vertices[current_vertex]

        return path[::-1]

    def shortest_santa_path(self) -> List[Any]:
        min_path = (inf, None,)
        n_permutations = math.factorial(len(self.vertices))
        for i, permutation in enumerate(itertools.permutations(self.vertices)):
            # print(f'{i+1}/{n_permutations}')
            path_length = self.path_length(permutation)
            if path_length < min_path[0]:
                min_path = (path_length, permutation,)

        return min_path[1]

    def path_length(self, path: List[Any]) -> float:
        if len(path) < 2: raise ValueError('Path length should be greater than 2.')
        prev_vertex = path[0]
        length = 0
        for current_vertex in path[1:]:
            if not prev_vertex in self.neighbours or not current_vertex in self.neighbours[prev_vertex]:
                raise AttributeError(f'Edge {prev_vertex} {current_vertex} not found in the graph.')
            length += self.neighbours[prev_vertex][current_vertex]
            prev_vertex = current_vertex

        return length
