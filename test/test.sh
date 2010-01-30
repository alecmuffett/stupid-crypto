#!/usr/local/bin/bash

#set -e
#set -x

LANG=$1

for a in *.stupid; do 
  BN=$(basename -s .stupid $a)
  echo -n "testing ${BN} in ${LANG} ... "
  PERLLIB=../src ./build-${LANG}.sh ${BN}
  if [ "$?" = "0" ] ; then 
    ./${BN} >/dev/null 2>&1
    if [ "$?" = "0" ] ; then 
      echo SUCCESS
    else
      echo FAIL: Execute
    fi
  else
    echo FAIL: Compile
    # don't exit here because want to keep going
  fi
  rm ${BN}
done

exit 0
