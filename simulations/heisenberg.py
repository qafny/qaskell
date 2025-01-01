"""Performs the simulation of the Heisenberg XXX model (Jx=Jy=Jz=J). We rely on the classical QuTiP simulator merely
for the convenience. Users can choose to utilize other available vendors as necessary.

Note: We do not include an example for XY model as it is a simplification of the Heisenberg model. Eliminate the
lattice-site interaction term (coupling term) along the Z direction, and it will result in the XY model"""

__author__ = "Chandeepa Dissanayake"

import networkx as nx
from simuq import QSystem
from simuq import Qubit
import helpers
import utils

# Since this is the XXX model, this suggests that the interaction strengths/coupling constants over all axes
# are equal. We set J = 1 for simplicity.
J = 1
# Similarly, we have assumed that the external magnetic field (h) affects every lattice site equally. We also set h =
# 1 for simplicity.
h = 1

# Create a 3D 2x2 lattice. Note: Change this to 3x3 (or anything higher) and try with classical simulators to realize
# why we need quantum hardware ;)
G = nx.grid_graph(dim=(2, 2, 2))
# Time duration for evolution
T = 1

# Convert the lattice from a coordinate-based index to integer-based index system
G = nx.convert_node_labels_to_integers(G)
# Number of lattice sites in our system.
N = G.number_of_nodes()

# SimuQ Stuff
qs = QSystem()
q = [Qubit(qs) for _ in range(N)]

# Starts building the Hamiltonian. First loop builds the lattice-site interaction term whereas the second builds up
# external field interaction term. We consider all the edges(tubes) in the lattice(visualization), as they denote the
# nearest neighbour interactions.
H = 0
for i, j in G.edges():
    H += -0.5 * J * (q[i].X * q[j].X + q[i].Y * q[j].Y + q[i].Z * q[j].Z)
# Loops over all lattice sites to add the external field interaction terms.
for i in range(N):
    H += -0.5 * h * q[i].Z

# Simulate the Hamiltonian H over time period T using QuTiP simulator
results = helpers.simulate_on_qutip(qs, H, T)
# Print the results in general format. It is evident that the state "000000" is the ground state, as flipping any
# qubit will result in an increment of the energy.
helpers.print_simulator_results(results, N)

# Visualize the lattice structure in 3D.
utils.visualize_lattice(G, 3)
