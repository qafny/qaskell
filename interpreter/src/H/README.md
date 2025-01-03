# Hamiltonians and simple Simulator

Hamiltonians and Simulator are being translated from the following Python Jupyter notebook, also authored by JBG:


        20241215_Ising_Hamiltonians.ipynb

Load necessary packages

        ghci> :set -package containers

        ghci> :set -package random

Load Simulator:

        ghci> :l H.Simulator 

Run test functions:

        ghci> :l H.NumberPartition 
        ghci> testNumberPartitionH

        ghci> :l H.GraphPartition
        ghci> testGraphPartitionH