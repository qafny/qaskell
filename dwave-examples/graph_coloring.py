from collections import defaultdict
import helpers
import networkx as nx


print_embedding = True

A = 2

n = 3
G = nx.Graph()
G.add_edges_from([
    (1, 2),(1, 4), (1, 5), (1, 6), (2, 3), (2, 4), (3, 4), (4, 5), (5, 6)
])


def main():
    Q = defaultdict(int)

    # Assign a qubit per each vertex, per each color.
    for _v in G.nodes():
        for i in range(n):
            v = _v - 1
            Q[((v * n) + i, (v * n) + i)] = -A  ## = -2A + A

    for _u, _v in G.edges():
        u = _u - 1
        v = _v - 1

        for i in range(n):
            Q[((u * n) + i, (v * n) + i)] = A  ## = A for each same color vertex pairs which are edges

    if print_embedding:
        embedding = helpers.get_minor_embedding(Q)
        print(embedding)

    response = helpers.run_on_dwave(Q, label="Graph Coloring", chainstrength=0, numruns=100)
    response_data = response.data(fields=['sample', 'energy', 'num_occurrences'])

    print('-' * 130)
    print('{:>70s}{:>30s}{:^30s}'.format('Colors List', 'Energy', 'Num Occurrences'))
    print('-' * 130)
    for sample, E, num_occurrences in response_data:
        colors_encoded = [x_vi for k, x_vi in sample.items()]

        colors = []
        for _v in G.nodes():
            v = _v - 1
            v_colors = colors_encoded[(v * n):((v * n) + n)]

            if 1 in v_colors:
                chosen_color = v_colors.index(1)
            else:
                chosen_color = -1

            colors.append(chosen_color)

        print('{:>70s}{:>30s}{:^30s}'.format(colors, str(E), num_occurrences))

    helpers.show_inspector(response)


if __name__ == '__main__':
    main()
