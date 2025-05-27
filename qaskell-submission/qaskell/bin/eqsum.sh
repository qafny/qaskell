#!/bin/bash

echo "EqSum Classical:"
cabal run qaskell -- eqsum classical

echo "EqSum Quantum:"
cabal run qaskell -- eqsum quantum | python scripts/parser.py > scripts/eqsum.py && python scripts/eqsum.py