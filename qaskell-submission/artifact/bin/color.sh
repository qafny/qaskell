#!/bin/bash

echo "Graph Color Classical:"
cabal run qaskell -- eqsum classical

echo "Graph Color Quantum:"
cabal run qaskell -- color quantum | python3 scripts/parser.py > scripts/color.py && python3 scripts/color.py