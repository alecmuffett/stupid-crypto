#!/usr/bin/env bash

set -e
#set -x

TARGET=$1

PERLLIB=../src ../src/stupid2.pl --debug --language=C ${TARGET}.stupid > generated/C/${TARGET}.c
gcc -Wall -Werror -o generated/C/${TARGET} test-wrapper.c generated/C/${TARGET}.c
