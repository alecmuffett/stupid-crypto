#!/usr/bin/env bash

set -e

TARGET=$1

../src/stupid.pl --language=Haskell ${TARGET}.stupid > ${TARGET}.hs
ghc -i../lib --make -o ${TARGET} ${TARGET}.hs 
