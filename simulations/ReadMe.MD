
# MuQ Examples

## Introduction
This project contains simulations of various quantum systems, which are part of our MuQ paper, written on top of the SimuQ Framework. The aim of this project is to supplement the ideas in our manuscript by providing a set of examples for the quantum systems we described. Each simulation will also feature its own Hamiltonian, which you will obtain upon compilation of a program written in MuQ.

## Simulations
Below is a brief description of each simulation included in this project.

### Ising Model
File: [ising.py](ising.py)

Simulation of the Ising model with equal interaction strength and external field strength over the  entire lattice.

### Heisenberg Model
File: [heisenberg.py](heisenberg.py)

Simulation of the Heisenberg XXX model (Jx=Jy=Jz=J). XY Model is implicitly included as it a special case of this model.

### Hubbard Model - One-Dimensional Hydrogen Chain
File: [1d_hydrogen_chain.py](1d_hydrogen_chain.py)

Simulation of the 1D Hydrogen Chain (Hubbard Model). We consider a chain with 2 Hydrogen atoms in the half-filling configuration (same number of sites as the particles).

### Bose-Hubbard Model - A Two-Particle Bosonic System
File: [bose_hubbard.py](bose_hubbard.py)

Simulation of a 2-particle bosonic system. As mentioned in the paper, this is based on the Bose-Hubbard model.

It is worthwhile to mention that this simulation is simply performed for a toy problem (arbitrary particle system), only with 2 particles for an arbitrary time period. These can be changed based on the needs of different systems. If it is required to introduce time dependent components to the Hamiltonian, that can be accommodated as well, but with appropriate changes. Refer to 1d_hydrogen_chain.py for a time-dependent simulation.

### t-J Model
File: [tj.py](tj.py)

Simulation of a N-particle t-J model using the creation-annihilation operator definition, using Fermion sites in SimuQ.

## Setup
To set up the project, follow these steps:

1. Clone the repository (If you already have the project files, skip this step):
    ```bash
    git clone https://github.com/liyili2/qaskell_impl.git
    cd qaskell_impl/simulations
    ```

2. Create and activate a virtual environment:
    ```bash
    python3 -m venv venv
    source venv/bin/activate  # On Windows use `venv\Scripts\activate`
    ```

3. Install the required dependencies:
    ```bash
    pip install -r requirements.txt
    ```

## Running the Simulations
In order to run any of the aforementioned simulations, make sure you've installed all the requirements and followed all setup-guidelines above. Then in order to run any example, execute,
```bash
python file_name.py
```

For example, if you need to run Ising model example,
```bash
python ising.py
```
