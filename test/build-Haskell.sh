#!/usr/local/bin/bash

set -e

TARGET=$1

../src/stupid.pl --language=Haskell ${TARGET}.stupid > ${TARGET}.hs 2> /dev/null
ghc -i../lib --make -o ${TARGET} ${TARGET}.hs
