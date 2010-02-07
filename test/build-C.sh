#!/usr/local/bin/bash

set -e
set -x

TARGET=$1

PERLLIB=../src ../src/stupid.pl --language=C ${TARGET}.stupid > generated/${TARGET}.c

