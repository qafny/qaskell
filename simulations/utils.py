import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
from mayavi import mlab


def visualize_lattice(G, dim):
    """
    Draws a networkX graph on a 3D plane using mayavi library.
    Code mostly adapted from: https://networkx.org/documentation/stable/auto_examples/3d_drawing/mayavi2_spring.html#sphx-glr-auto-examples-3d-drawing-mayavi2-spring-py
    :param G: A NetworkX Graph instance
    :param dim: Number of dimensions to visualize the graph in. Typically, a 3D lattice is expected to be drawn in the 3D plane.
    :return: None
    """

    if dim < 3:
        nx.draw(G)
        plt.show()
    else:
        G_c = nx.convert_node_labels_to_integers(G)  # Convert integer coordinate node based graph to an index based node
        # graph.
        pos = nx.spring_layout(G_c, dim=dim, seed=1001)  # Converts the integral nodes to coordinates in 3D plane (
        # position nodes) using Fruchterman-Reingold force-directed algorithm (Documentation:
        # https://networkx.org/documentation/stable/reference/generated/networkx.drawing.layout.spring_layout.html)
        xyz = np.array([pos[v] for v in sorted(G_c)])  # Sorts the 3D-plane coordinates (nodes) for plotting. This is
        # merely a requirement in mayavi plotting.
        scalars = np.array(list(G_c.nodes())) + 5  # Scalar value per each node, to modulate their size and color.

        mlab.figure()  # Starts the visualization using Mayavi
        pts = mlab.points3d(
            xyz[:, 0],
            xyz[:, 1],
            xyz[:, 2],
            scalars,
            scale_factor=0.1,
            scale_mode="none",
            colormap="Blues",
            resolution=20,
        )  # Documentation: https://docs.enthought.com/mayavi/mayavi/auto/mlab_helper_functions.html#points3d
        pts.mlab_source.dataset.lines = np.array(list(G_c.edges()))  # Sets all edges as lines in the 3D space.
        tube = mlab.pipeline.tube(pts, tube_radius=0.01)  # Converts the edges to a tube-like visual structure
        mlab.pipeline.surface(tube, color=(0.8, 0.8, 0.8))  # Performs the actual visualize of edges as tubes on the window (scene)
        mlab.orientation_axes()  # Creates 3D axes and marks the coordinates

        mlab.show()  # This blocks the event loop.
