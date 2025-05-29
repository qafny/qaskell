#!/bin/bash

docker build -t qaskell .
docker run --rm qaskell /qaskell/qaskell-submission/qaskell/eqsum.sh
docker run --rm qaskell /qaskell/qaskell-submission/qaskell/clique.sh
docker run --rm qaskell /qaskell/qaskell-submission/qaskell/infer.sh