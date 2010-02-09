#!/bin/sh

make

if [ "$?" == "0" ] ; then

./stupid-haskell-sha256sum < testvector.txt  > testsha256.tmp

diff --brief testsha256.tmp testsha256.out

if [ "$?" == "0" ]; then
  echo stupid-haskell-sha256sum test success
  exit 0
else
  echo stupid-haskell-sha256sum test failed - output wrong
  exit 1
fi
else
  echo stupid-haskell-sha256sum test failed - build failed
fi
