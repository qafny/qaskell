# Hamiltonians

## /notebooks

Hamiltonians and Simulator are being translated from the following Python Jupyter notebook, also authored by JBG:


        20241215_Ising_Hamiltonians.ipynb

        20241224_Hamiltonian_Simulator.ipynb

## /Imp (imparative)

Pretty much a direct Haskell translation of the Python code in the notebooks. "Imp" is short for "imperative", it's not imperative, but the pattern more or less "feels" that way.

## /Comp (functional)

An iteration of the `/Imp` Solver, "Comp" for composition, but in a similar functional pipeline pattern of what is in DSL. Meaning:

`minimize` ◦ `solve_` ◦ `generateSpins`

However, only really works equally well for both classical and quantum versions, where the Hamiltonian in the quantum case results in a diagonalized matrix.

## /EnQ

Requested implementations from Prof. Li.


### Notes (I've put into Slack)

I'm pretty deep in the weeds here but have really learned a ton and found some surprising conclusions, to be honest.

Probably the most interesting is an implementation of the Childs clique-finding algorithm, as requested by Prof. Li:  
Childs, Andrew M., Edward Farhi, Jeffrey Goldstone, and Sam Gutmann. “Finding Cliques by Quantum Adiabatic Evolution.” *Quantum Information and Computation* 2, no. 3. Accessed December 31, 2024. https://doi.org/10.26421/QIC2.3.  

It's in `H/EnQ/Clique.hs`.

A few things of note here:

- I find the fact that \( H_P \) still returns sets, with a dynamic \( k \) that may not be a clique, deeply unsatisfying. So, I opted to continue reducing \( k \) in iterative steps until I have a result that was also discovered classically. This works.

- I simply could not get a positive energy gap for quantum simulation with a calculated value of \( T \), so it's static at the moment (\( T = 10 \)).

- The Childs quantum method doesn't immediately follow our composition pattern. It does claim to have a quantum advantage, but I'm not sure which aspect is most important to us.

- The classical calculation follows our composition pattern: `minimize` ◦ `solveClassical` ◦ `generateChoices`. However, it does not currently use a classical Hamiltonian. There is a working classical Hamiltonian in `H/Comp/Cliques.hs`, but that is unrelated to the Childs paper. Maybe I can bring it over or something—it's on my to-do list.

Zooming out from that implementation specifically, here are some other things I discovered personally:

I'm not totally sure what our primary objective is, but the more in the weeds I go, the more I think the things I find super interesting are maybe, in some ways, mutually exclusive—or at least will take more research to figure out how to bring them closer. Here's a list:

- In `H/Comp`, I guess my "dream" was to define universal Hamiltonians from the Ising paper that could be "compiled" to run classically or simulated quantumly. It turns out this does work, provided that in the quantum case the Hamiltonians, as they evolve, are diagonal. However, if the Hamiltonian is more complex with off-diagonal terms, it won't work and would require a Hamiltonian specifically for the quantum case.

- There are also some options for `generateChoices`. I started off thinking about partition choices, vertex choices, and edge choices, but `H/Comp` is doing spin configurations. This is maybe a little more universal from an Ising spin perspective, and providing appropriate Hamiltonians does solve problems.

- In the EnQ paper, it explicitly mentions "quantum circuits." None of this work is circuit-based—it is all based on adiabatic quantum simulation. Not sure if this is a problem.
