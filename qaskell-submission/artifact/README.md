# EnQ Artifact

TITLE: A Haskell Adiabatic DSL: Solving Classical Optimization Problems on Quantum Hardware  
ICFP SUBMISSION NO: 38

# METADATA

- OS and resources (CPU, memory, disk, GPU) used by the authors to run the artifact

We provide a single script to build a Docker image and run each example in a temporary self-contained container based on the built image.

The constructed "qaskell" image size is 10.48 GB.

The artifact was tested with Docker v4.17.0 on macOS v15.5.

# Haskell version INFO

GHC 9.12.2
cabal-install version 3.12.1.0
compiled using version 3.12.1.0 of the Cabal library

# Quick Start

From the `artifact/` directory execute `run.sh`.

## Build & Run

The whole process to build & run examples takes <10 mins.

        $ ./run.sh

This will build a container and run the following examples both classically and as a quantum Qiskit simulation.

Example for EqSum also emits a D-Wave script. D-Wave targets require credentials to run on Dwave hardware.

## Equal Sum Example

Equal Sum on a small list `[1, 3, 2]`

### Equal Sum - Classical

The Haskell solution can be run as follows:

        $ cabal run qaskell -- eqsum classical

Or if you prefer to use a REPL and test other data structures:

        $ cabal repl

        ghci> :m Quantum.Examples Quantum.Program Quantum.Examples

        ghci> print $ solveClassical (eqSum [1,3,2])

#### Expected result

For this simple case, 1 and 2 should be paired together, and 3 should be on its own. There are 2 valid solutions.

        [(-7,[(1,-1),(3,1),(2,-1)]),(-7,[(1,1),(3,-1),(2,1)])]

### Equal Sum - Quantum

The Haskell solution that generates Pauli strings can be run as follows:

        $ cabal run qaskell -- eqsum quantum

Or if you prefer to use a REPL and test other data structures:

        $ cabal repl

        ghci> :m Quantum.Examples Quantum.Program Quantum.Examples

        ghci> print $ solveQuantum (eqSum [1,3,2])

#### Expected result

Similar to the classical case, the 2 valid solutions for quantum (101 and 010) are as follows.

                {'110': 134, '101': 181, '100': 86, '001': 149, '000': 116, '111': 100, '010': 180, '011': 78}

Consider quantum theory is inherently probabilistic, so all configurations are possible outputs at measurement, so we run the algorithm many times, and see which results are measured most often. Here 010 and 101.

## Graph Coloring

Graph coloring using the graph:

                1 --- 2
                 \   /
                  \ /
                   3
                    \
                     4 --- 5

### Graph Coloring - Classical

The Haskell solution can be run as follows:

        $ cabal run qaskell -- color classical

Or if you prefer to use a REPL to test other data structures and color counts:

        $ cabal repl

        ghci> :m Quantum.Examples Quantum.Program Quantum.Examples Quantum.ExampleData

        ghci> print $ solveClassical (graphColoring 2 graph4)

There are other graphs in `ExampleData`: graph1, graph2, graph3, graph4, graph6, graph7, graph8

#### Expected result

There are 2 valid solutions for k = 2 (number of colors), and the example graph above. The colors must alternate.

        [(0,[(1,0),(2,1),(3,0),(4,1),(5,0)]),(0,[(1,1),(2,0),(3,1),(4,0),(5,1)])]

### Graph Coloring - Quantum

Similar to the classical instructions, just simply:

        $ cabal run qaskell -- color quantum

or

        ghci> print $ solveQuantum (graphColoring 2 graph4)

#### Expected result

The expected result matches the classical solution; however, as with the quantum eqsum example, multiple samples/measurements are required due to the probabilistic nature of quantum computation.

        {'01010': 153, '10101': 179, '00111': 22, '10110': 44, '00110': 38, '01001': 50, '10010': 47, '01101': 49, '11100': 31, '01011': 15, '11001': 36, '01100': 33, '00001': 10, '11000': 20, '11111': 16, '10100': 25, '11011': 43, '00101': 29, '01110': 11, '11010': 18, '10001': 12, '00011': 21, '10011': 40, '00000': 17, '01111': 6, '00100': 29, '11110': 10, '11101': 8, '00010': 1, '10111': 4, '10000': 5, '01000': 2}

Note that only '01010' and '10101' represent valid colorings in this case; the other bitstrings correspond to invalid or suboptimal solutions. Counts are subject to individual experimental results due to the probabilistic nature of quantum mechanics.

## Clique Finding

Clique finding on the graph:

                1 --- 2    4
                |    /     |
                |   /      |
                |  /       |
                | /        |
                3          5

### Clique Finding - Classical

The Haskell solution can be run as follows:

        $ cabal run qaskell -- clique classical

Or if you prefer to use a REPL to test other data structures and clique sizes:

        $ cabal repl

        ghci> :m Quantum.Examples Quantum.Program Quantum.Examples Quantum.ExampleData

        ghci> print $ solveClassical (cliqueFinding graph1)

There are other graphs in `ExampleData`: graph1, graph2, graph3, graph4, graph6, graph7, graph8

#### Expected result

Using the graph above, for example, 1, 2, 3 should be grouped together as they form a clique.

        [(0,[(1,1),(2,1),(3,1),(4,0),(5,0)])]

### Clique Finding - Quantum

Similar to the classical instructions, just simply:

        $ cabal run qaskell -- clique quantum

or

        ghci> print $ solveQuantum (cliqueFinding graph3)

#### Expected result

There are 2 valid quantum solutions with high probability to clique finding: 00111 and 11000. Note: Qiskit is little endian, bit strings should be read backwards.

## Type Inference

Type inference with `(𝜆𝑥 : Int.𝑥)𝑦`

### Type Inference - Classical

The Haskell solution can be run as follows:

        $ cabal run qaskell -- infer classical

Or if you prefer to use a REPL and test other expressions:

        $ cabal repl

        ghci> :m Quantum.Examples Quantum.Program Quantum.Examples Quantum.ExampleData

        ghci> print $ solveClassical (inferType exprFull)

#### Expected result

There are 2 types in the expression `(𝜆𝑥 : Int.𝑥)𝑦`, Int and Int → Int 

        [(0,MaybeExpr {unMaybeExpr = Just (App (Just (Lambda "x" IntType (Just (Var "x" ((),IntType))) ((),IntType :-> IntType),Var "y" ((),IntType))) ((),IntType))})]

### Type Inference - Quantum

Similar to the classical instructions, just simply:

        $ cabal run qaskell -- infer quantum

or

        ghci> print $ solveQuantum (inferType exprFull)

#### Expected result

There are 2 types in the expression `(𝜆𝑥 : Int.𝑥)𝑦`, Int and Int → Int. Qiskit is little endian, so the expected solution is the bitstring '00000100', with high probability.

## Contents

| *run.sh*                  | Build & run Docker container and examples                     |
|---------------------------|---------------------------------------------------------------|
| *README.md*               | This README file                                              |
| *Dockerfile*              | Docker container definition                                   |
| *requirements.txt*        | Python requirements to run quantum examples and simulations   |
| *bin/*                    | Bash scripts for Docker to run examples                       |
| *../notebooks/*           | Python notebooks for running quantum examples                 |
| *../qaskell/*             | The Haskell Cabal project                                     |
