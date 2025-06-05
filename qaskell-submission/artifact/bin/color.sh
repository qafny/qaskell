#!/bin/bash

target="$1"

echo "Graph Color Classical:"
cabal run qaskell -- color classical

echo "Graph Color Quantum:"
cabal run qaskell -- color quantum | python3 scripts/${target}/parser.py > scripts/${target}/color.py && python3 scripts/${target}/color.py