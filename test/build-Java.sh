#!/usr/bin/env bash

set -e
#set -x

TARGET=$1

PERLLIB=../src ../src/stupid.pl --language=Java ${TARGET}.stupid > generated/${TARGET}.java
javac generated/${TARGET}.java ../lib/Stupid/*java

