#!/bin/bash

source /root/.ghcup/env
ghcup set ghc latest

echo "EqSum Classical:"
cabal run qaskell -- eqsum classical

echo "EqSum Quantum:"
cabal run qaskell -- eqsum quantum | python3 scripts/parser.py > scripts/eqsum.py  && python3 scripts/eqsum.py