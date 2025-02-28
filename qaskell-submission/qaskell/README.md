# Building

In order to build the project, run `cabal build`.

# Testing

To test out example programs, first run `cabal repl`. Then, run the following three commands:

    ghci> import Quantum.Program
    ghci> import Quantum.Examples
    ghci> import Quantum.ExampleData

Then, you can test out the quantum compilation like this:

    ghci> solveQuantum (eqSum [1,2,3])

The classical interpretation can be run using:

    ghci> solveClassical (eqSum [1,2,3])

The examples consist of `eqSum`, `graphColoring`, `cliqueFinding`, `exactCover`, and `inferType`.

Example graphs can be found in `src/Quantum/ExampleData.hs`.

For example, the quantum compilation for graph coloring with two colors on `graph1` can be run like this:

    ghci> solveQuantum (graphColoring 2 graph1)

