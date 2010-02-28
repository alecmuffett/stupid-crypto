#!/usr/bin/env bash

set -e

TARGET=$1

PERLLIB=../src ../src/stupid.pl --language=Haskell ${TARGET}.stupid > generated/${TARGET}.hs
ghc -i../lib --make -o generated/${TARGET} generated/${TARGET}.hs 
