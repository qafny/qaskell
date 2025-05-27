#!/bin/bash

echo "Graph Color Classical:"
cabal run qaskell -- eqsum classical

echo "Graph Color Quantum:"
cabal run qaskell -- color quantum | python scripts/parser.py > scripts/color.py && python scripts/color.py