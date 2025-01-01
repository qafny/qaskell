"""Performs the simulation of the 1D Hydrogen Chain (Hubbard Model). We consider a chain with 2 Hydrogen atoms in the
half-filling configuration (same number of sites as the particles).

This example is written in Pauli gates formulation.

The values of z_t and z_u are set arbitrarily. Increase the time period for simulation as required. We rely on the
classical QuTiP simulator merely for the convenience. Users can choose to utilize other available vendors as
necessary."""

__author__ = "Chandeepa Dissanayake"

import networkx as nx
from simuq import QSystem, Qubit
import helpers
import utils

# The number of Hydrogen atoms in the chain.
N = 2
# The hopping integral. Taken to be positive.
z_t = 1
# Initial and Final values for On-site interaction strength. Can be either positive or negative.
# For further specifications on simulating a Hubbard system to evaluate phase changes:
z_u0 = -5
z_uf = 5
# Time duration for evolution
T = 4
# Number of discretization steps
M = 30

# Create the 1D Hydrogen Chain with N sites.
G = nx.Graph()
# Edges denote interactions between neighbouring sites. Since this is a chain, neighbours are right next to each
# other on 1D plane.
G.add_edges_from([(i + 1, i + 2) for i in range(N - 1)])
# Convert the lattice from a coordinate-based index to integer-based index system
G = nx.convert_node_labels_to_integers(G)

# SimuQ Stuff
qs = QSystem()
q = [Qubit(qs) for _ in range(N)]

# Starts building the Hamiltonian.
H_t = 0
H_u = 0
for i, j in G.edges():
    H_t += q[i].X + q[j].X
    H_u += q[i].Z * q[j].Z

# Build the Time Dependent Hamiltonian
H = lambda t: -z_t * H_t + ((1 - t / T) * z_u0 + (t / T) * z_uf) * H_u

# Simulate the Hamiltonian H over time period T using QuTiP simulator
results = helpers.simulate_on_qutip(qs, H, T, time_dependent=True, M=M)
# Print the results in general format.
helpers.print_simulator_results(results, N)

# Visualize the lattice structure in 3D.
utils.visualize_lattice(G, 1)
