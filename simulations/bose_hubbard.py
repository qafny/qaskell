"""Performs the simulation of the 2-particle bosonic system. As mentioned in the paper, this is based on the
Bose-Hubbard model.

It is worthwhile to mention that the subsequent simulation is simply performed for a toy problem (arbitrary particle
system), only with 2 particles for an arbitrary time period. These can be changed based on the needs of different
systems. If it is required to introduce time dependent components to the Hamiltonian, that can be accommodated as
well, but with appropriate changes. Refer to 1d_hydrogen_chain.py for a time-dependent simulation.

The purpose of this example is solely to demonstrate how to write the Hamiltonian. Moreover, the Hamiltonian can
significantly change (increase in complexity) when dealing with a large number of particles.

Further note that quadruple site-interactions is not the best way to write Hamiltonian expressions for the current
systems. It is possible to re-write the given Hamiltonian as two-site interactions, but we will leave it for an
interested reader to pursue."""

__author__ = "Chandeepa Dissanayake"

import networkx as nx
from simuq import QSystem, Qubit
import helpers
import utils

# Time duration for evolution. Used an arbitrary value.
T = 4

# SimuQ Stuff
qs = QSystem()
q1, q2 = Qubit(qs), Qubit(qs)

# Build the Time Dependent Hamiltonian
H = (1.0 / 8.0) * (
        (q1.X * q2.X * q1.X * q2.X) +
        (q1.X * q2.X * q1.Y * q2.Y) +
        (q1.Y * q2.Y * q1.X * q2.X) +
        (q1.Y * q2.Y * q1.Y * q2.Y) +
        (q1.X * q2.Y * q1.X * q2.Y) +
        (q1.X * q2.Y * q1.Y * q2.X) +
        (q1.Y * q2.X * q1.X * q2.Y) +
        (q1.Y * q2.X * q1.Y * q2.X)
)

# Simulate the Hamiltonian H over time period T using QuTiP simulator
results = helpers.simulate_on_qutip(qs, H, T)
# Print the results in general format.
helpers.print_simulator_results(results, 2)
