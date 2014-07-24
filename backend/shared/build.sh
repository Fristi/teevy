#!/bin/bash

cd "$(dirname "$0")"

cabal clean
cabal configure
cabal install --only-dependencies --force-reinstalls
cabal install