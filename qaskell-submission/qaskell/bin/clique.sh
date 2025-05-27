#!/bin/bash

echo "Clique Classical:"
cabal run qaskell -- clique classical

echo "Clique Quantum:"
cabal run qaskell -- clique quantum | python scripts/parser.py > scripts/clique.py && python scripts/clique.py