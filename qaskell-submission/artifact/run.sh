#!/bin/bash

docker build -t qaskell .
docker run --rm qaskell /qaskell/qaskell-submission/qaskell/eqsum.sh "qiskit"
docker run --rm qaskell /qaskell/qaskell-submission/qaskell/eqsum.sh "dwave"
docker run --rm qaskell /qaskell/qaskell-submission/qaskell/clique.sh "qiskit"
docker run --rm qaskell /qaskell/qaskell-submission/qaskell/infer.sh "qiskit"