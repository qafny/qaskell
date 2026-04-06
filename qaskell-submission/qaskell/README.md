# Changes

1. Fixed bugs in Python scripts that parameters are not scaled by time interpolation

2. Added sorting program `sortP` in `Examples.hs`,  the generated qiskit circuits does not solve sorting, currently under investigation

3. Added module `Analysis.hs` to print out program info such as energy table, optimal solutions, etc. The `preprocess` function extend programs that has less than 2^n choices (illegal states will be created by binary encoding) with penalties targeting illegal states

4. Added commands in Main

# Quick Run

for `solveQuantum $ preprocess (sortP [2,1,3])`:

cabal run qaskell -- sort3 quantum | python3 scripts/qiskit/parser.py > sort213.py


