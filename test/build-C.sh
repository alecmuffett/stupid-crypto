#!/usr/bin/env bash

set -e
#set -x

TARGET=$1

PERLLIB=../src ../src/stupid.pl --language=C ${TARGET}.stupid -debug > generated/C/${TARGET}.c
gcc -Wall -Werror -o generated/C/${TARGET} test-wrapper.c generated/C/${TARGET}.c
