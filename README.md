# EnQ

The EnQ project intends to develop a new computation model for quantum computing including the circuit-based model as well as analog quantum computing models.

## Overview

Quadratic unconstrained binary optimization (QUBO) and quantum adiabatic computation (QAC) are two computational models for describing quantum algorithms, which are different from the traditional circuit-based models. They describe quantum algorithms in terms of energy constraints described as Hamiltonians in physical models. QAC currently has no programming language supports while QUBO is used to describe classical energy optimizations, without a clear picture of how it is connected to quantum computation. In this paper, we present EnQ, a programming language that is suitable to describe both QUBO and QAC. In EnQ, we categorically identify abstract operations in both QUBO and QAC, and connect operations in these two different computation models in a category theory, which can be instantiated to the computation for the two models.
Additionally, we identify the computation pathways for the two models and encode them in the EnQ executions to crystallize energy constraint optimizations can happen.
Finally, we provide compilation from EnQ to quantum circuits, with case studies of using EnQ to solve classical NP programs, such as number partitioning and clique finding.

## Content

The artifacts are in the three directories, and there are instructions in each directory.

* coq directory contains the coq definitions and proofs for theorems in the paper.

* interpreter directory contains the interpreter for the classical energy computation in 3.1.

* simulations contains the python files run on SimuQ.
