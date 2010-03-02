#!/usr/bin/env bash

set -e

TARGET=$1

PERLLIB=../src ../src/stupid.pl --language=Haskell ${TARGET}.stupid > generated/Haskell/${TARGET}.hs
ghc -i../lib --make -o generated/Haskell/${TARGET} generated/Haskell/${TARGET}.hs 
