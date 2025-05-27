#!/bin/bash

docker build -t qaskell .
docker run --rm qaskell /qaskell/qaskell-submission/qaskell/eqsum.sh