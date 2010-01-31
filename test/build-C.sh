#!/usr/local/bin/bash

set -e
set -x

TARGET=$1

../src/stupid.pl --language=C ${TARGET}.stupid > ${TARGET}.c 2> /dev/null
gcc -g -Wall -Werror -o ${TARGET} ${TARGET}.c
