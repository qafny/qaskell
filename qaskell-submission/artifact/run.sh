#!/bin/bash

target="$1"

docker build -t qaskell .
docker run --rm qaskell /qaskell/qaskell-submission/qaskell/eqsum.sh "$target"
docker run --rm qaskell /qaskell/qaskell-submission/qaskell/clique.sh "$target"
docker run --rm qaskell /qaskell/qaskell-submission/qaskell/infer.sh "$target"