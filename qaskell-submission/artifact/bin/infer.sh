#!/bin/bash

target="$1"

source /root/.ghcup/env
ghcup set ghc latest

echo "inferType Classical:"
cabal run qaskell -- infer classical

echo "inferType Quantum:"
cabal run qaskell -- infer quantum | python3 scripts/${target}/parser.py > scripts/${target}/infer.py && python3 scripts/${target}/infer.py