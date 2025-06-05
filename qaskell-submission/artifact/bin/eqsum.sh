#!/bin/bash

target="$1"

source /root/.ghcup/env
ghcup set ghc latest

echo "EqSum Classical:"
cabal run qaskell -- eqsum classical

cmd="python3"
if [ "$target" = "dwave" ]; then
    cmd="cat"
fi

echo "EqSum Quantum:"
cabal run qaskell -- eqsum quantum | python3 scripts/${target}/parser.py > scripts/${target}/eqsum.py && ${cmd} scripts/${target}/eqsum.py