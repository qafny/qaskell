# EnQ Aritifact

TITLE: A Haskell Adiabatic DSL: Solving Classical Optimization Problems on Quantum Hardware
ICFP SUBMISSION NO: 38


# METADATA

- OS and resource (CPU, memory, disk, GPU) used by the authors to run the artifact

We provide a single script to build a Docker image, and run each example in a temporary self contained container based on the built image.

The constructed "qaskell" image size is 10.48 GB.

Artifact was tested with Docker v4.17.0 on macOS v15.5.

# Quick Start

From the `artifact/` directory execute `run.sh`.

## Build & Run

The whole process to build & run examples takes <10 mins.

                $ ./run.sh

This will build a container and run the following examples both classically and as a quantum qiskit simulation.

## Equal Sum Example

* Equal Sum on a small list `[1, 3, 2]`

### Equal Sum Classical

Haskell solution can be run as follows:

        $ cabal run qaskell -- eqsum classical

Or if you prefer to use a REPL, and test other data structures

        $ cabal repl

        ghci> :m Quantum.Examples Quantum.Program Quantum.Examples

        ghci> print $ solveClassical (eqSum [1,3,2])

### Equal Sum Quantum

Haskell solution that generates Pauli strings can be run as follows

        $ cabal run qaskell -- eqsum quantum

Or if you prefer to use a REPL, and test other data structures

        $ cabal repl

        ghci> :m Quantum.Examples Quantum.Program Quantum.Examples

        ghci> print $ solveQuantum (eqSum [1,3,2])

## Graph coloring

Graph coloring using the graph

                A --- B
                 \   /
                  \ /
                   C
                    \
                     D --- E

### Graph coloring - Classical

Haskell solution can be run as follows:

        $ cabal run qaskell -- color classical

Or if you prefer to use a REPL, test other data structures, and color counts

        $ cabal repl

        ghci> :m Quantum.Examples Quantum.Program Quantum.Examples Quantum.ExampleData

        ghci> print $ solveClassical (graphColoring 2 graph4)

There are other graphs in `ExampleData`: graph1, graph2, graph3, graph4, graph6, graph7, graph8

### Graph coloring - Quantum

Similar to the Classical instructions, just simply

         $ cabal run qaskell -- color quantum

or

        ghci> print $ solveQuantum (graphColoring 2 graph4)

## Clique Finding

Clique Finding on the graph

                A --- B    D
                |    /     |
                |   /      |
                |  /       |
                | /        |
                C          E

### Clique finding - Classical

Haskell solution can be run as follows:

        $ cabal run qaskell -- clique classical

Or if you prefer to use a REPL, test other data structures, and clique sizes

        $ cabal repl

        ghci> :m Quantum.Examples Quantum.Program Quantum.Examples Quantum.ExampleData

        ghci> print $ solveClassical (cliqueFinding 3 graph1)

There are other graphs in `ExampleData`: graph1, graph2, graph3, graph4, graph6, graph7, graph8

### Clique finding - Quantum

Similar to the Classical instructions, just simply

         $ cabal run qaskell -- clique quantum

or

        ghci> print $ solveQuantum (cliqueFinding 3 graph4)

## Type Inference

Type Inference with `𝜆𝑥 : Int.𝑥`

### Type Inference - Classical

Haskell solution can be run as follows:

        $ cabal run qaskell -- infer classical

Or if you prefer to use a REPL, and test other expressions

        $ cabal repl

        ghci> :m Quantum.Examples Quantum.Program Quantum.Examples Quantum.ExampleData

        ghci> print $ solveClassical (inferType exprFull)

### Type Inference - Quantum

Similar to the Classical instructions, just simply

         $ cabal run qaskell -- clique quantum

or

        ghci> print $ solveQuantum (inferType exprFull)

## Contents

| *run.sh*                  | Build & Run Docker container and examples                     |
| *README.md*               | This README file                                              |
| *Dockerfile*              | Docker container definition                                   |
| *requirements.txt*        | Python requirements to run quantum examples and simulations   |
| *bin/*                    | BASH scripts for Docker to run examples                       |
| *../notebooks/*           | Python notebooks for running quantum examples                 |
| *../qaskell/*             | The Haskell cabal project                                     |