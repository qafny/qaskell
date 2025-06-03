#!/bin/bash

target="$1"

source /root/.ghcup/env
ghcup set ghc latest

echo "EqSum Classical:"
cabal run qaskell -- eqsum classical

echo "EqSum Quantum:"
cabal run qaskell -- eqsum quantum | python3 scripts/${target}/parser.py > scripts/${target}/eqsum.py  && python3 scripts/${target}/eqsum.py