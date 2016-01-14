#!/bin/bash

# Why `process` takes 100% CPU, waiting on readFile?

ghc -fforce-recomp --make process.hs -O2 -Wall -threaded -rtsopts -with-rtsopts=-N -o process
rm -rf Buildsome.mk.db bla X
time buildsome bla -j8
