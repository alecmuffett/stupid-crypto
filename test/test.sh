#!/usr/bin/env bash

#set -e
#set -x

trap onexit HUP INT QUIT TERM

function onexit() {
    local exit_status=${1:-$?}
    echo Exiting $0 with status $exit_status
    rm /tmp/stupid.$$
    exit $exit_status
}

LANG=$1

for a in *.stupid; do 
  BN=$(basename -s .stupid $a)
  echo -n "testing ${BN} in ${LANG} ... "
  ./build-${LANG}.sh ${BN} >/tmp/stupid.$$ 2>&1
  if [ "$?" = "0" ] ; then
    ./${BN} >/dev/null 2>&1
    if [ "$?" = "0" ] ; then
      echo SUCCESS
    else
      echo FAIL: Execute
    fi
  else
    echo FAIL: Compile
    cat /tmp/stupid.$$
  fi
  rm /tmp/stupid.$$
done

exit 0
