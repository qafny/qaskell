from simuq import QSystem
from typing import Any
from simuq.qutip import QuTiPProvider
import numpy as np


def simulate_on_qutip(qs: QSystem, H: Any, T: int, time_dependent: bool = False, M: int = 0):
    """
    Performs a simulation using QuTiP Provider for the Hamiltonian H over a time period T and returns the results in
    the standard format of SimuQ Simulators.
    :param qs: Quantum System of Interest
    :param H: The Hamiltonian
    :param T: Time Period for Simulation
    :param M: Discretization Steps The number of steps that the time dependent Hamiltonian is to be discretized into.
    :param time_dependent: Is the given Hamiltonian time dependent?
    :return: A dictionary with the results of the simulation. Keys correspond to output
    states whereas the values to probabilities of observing corresponding states at the end of the simulation.
    """
    if time_dependent:
        qs.add_td_evolution(H, np.linspace(0, T, M))
    else:
        qs.add_evolution(H, T)

    qtpp = QuTiPProvider()
    qtpp.compile(qs)
    qtpp.run()
    results = qtpp.results()

    return results


def print_simulator_results(results: dict, N: int):
    """
    Display the results; First all probabilities and then the probability of ground state.
    :param results: SimuQ Simulator Results.
    :param N: Number of lattice sites
    :return: None
    """
    # Display the results; First all probabilities and then the probability of ground state.
    print(results)
    print("Probability of obtaining the ground state {state}: {prob}".format(
        state="0" * N,
        prob=results["0" * N]
    ))
