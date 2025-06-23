#!/bin/bash

target="$1"

source /root/.ghcup/env
ghcup set ghc latest

echo "Clique Classical:"
cabal run qaskell -- clique classical

echo "Clique Quantum:"
cabal run qaskell -- clique quantum | python3 scripts/${target}/parser.py > scripts/${target}/clique.py is_clique && python3 scripts/${target}/clique.py