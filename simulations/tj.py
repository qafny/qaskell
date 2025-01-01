__author__ = "Jae Swanepoel"

from simuq.environment import Fermion
from simuq.qsystem import QSystem
import numpy as np


# computes and returns the spin operator at a given site
# up, down: the respective up and down fermions for a site
def spin_op(up, down):
    return np.array([
        (up.a * down.c) + (down.a * up.c),
        1j * ((down.a * up.c) - (up.a * down.c)),
        (up.a * up.c) - (down.a * down.c)
    ]).transpose() / 2  # return row vector


def gen_tj(num_sites, time, t, J):

    N = 2 * num_sites

    qs = QSystem()  # instantiate base quantum system
    fermions = [Fermion(qs) for _ in range(N)]  # instantiate fermions in the system

    H = None

    for i in range(num_sites - 1):

        j = i + 1

        # up and down fermions at each site
        i_up, i_down = fermions[i * 2], fermions[(i * 2) + 1]
        j_up, j_down = fermions[j * 2], fermions[(j * 2) + 1]

        # c and a operators for each site
        ci = np.array([i_up.c, i_down.c]).transpose()
        ai = np.array([i_up.a, i_down.a])
        cj = np.array([j_up.c, j_down.c]).transpose()
        aj = np.array([j_up.a, j_down.a])

        t_term = -t * (np.dot(ai, cj) + np.dot(aj, ci))
        J_term = J * (np.dot(spin_op(i_up, i_down), spin_op(j_up, j_down)))

        if H is None:
            H = t_term + J_term
        else:
            H += t_term + J_term

    qs.add_evolution(H, time)
    return qs
