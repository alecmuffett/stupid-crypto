#!/usr/bin/env bash

set -e
#set -x

TARGET=$1

PERLLIB=../src ../src/stupid.pl --language=Java ${TARGET}.stupid > generated/Java/${TARGET}.java
javac generated/Java/${TARGET}.java ../lib/Stupid/*java

# the above builds the general code
# but now we'll also make some test harness code

sed "s/__TARGET__/${TARGET}/g" < TestWrapper.java > generated/Java/Test${TARGET}.java
javac generated/Java/Test${TARGET}.java generated/Java/${TARGET}.java ../lib/Stupid/*java

# and an executable to run that test:

echo "java -cp generated/Java:../lib/ Test${TARGET}" > generated/Java/${TARGET}
chmod a+x generated/Java/${TARGET}

