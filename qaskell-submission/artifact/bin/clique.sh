#!/bin/bash

source /root/.ghcup/env
ghcup set ghc latest

echo "Clique Classical:"
cabal run qaskell -- clique classical

echo "Clique Quantum:"
cabal run qaskell -- clique quantum | python3 scripts/parser.py > scripts/clique.py && python3 scripts/clique.py