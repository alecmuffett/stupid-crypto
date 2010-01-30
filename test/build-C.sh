#!/usr/local/bin/bash

set -e

TARGET=$1

../src/stupid.pl --language=C ${TARGET}.stupid > ${TARGET}.c 2> /dev/null
gcc -g -Wall -Werror -o ${TARGET} ${TARGET}.c
