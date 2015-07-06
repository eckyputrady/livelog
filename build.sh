#! /bin/bash

cabal sandbox init
cabal update
cabal install --only-dependencies --enable-tests
cabal test
cabal build