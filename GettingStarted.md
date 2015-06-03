# Introduction #

This is an example of building Stupid, and then running the stupid tests on Ben Clifford's OS X laptop.


# Details #

```
$ cd src
$ make
$ export PERL5LIB=$(pwd):$PERL5LIB

$ cd ../test

$ ./test.sh Haskell
testing array-small in Haskell ... SUCCESS
testing array-write in Haskell ... SUCCESS
testing array in Haskell ... SUCCESS
testing array2 in Haskell ... SUCCESS
testing assign in Haskell ... SUCCESS
testing assign2 in Haskell ... SUCCESS
testing comment-start in Haskell ... FAIL: Compile
testing function in Haskell ... SUCCESS
testing int in Haskell ... SUCCESS
testing int2 in Haskell ... SUCCESS
testing t1 in Haskell ... SUCCESS
testing t1p100 in Haskell ... SUCCESS
testing t1p15 in Haskell ... SUCCESS
testing t1p35 in Haskell ... SUCCESS
testing t1p70 in Haskell ... SUCCESS
testing t1p72 in Haskell ... SUCCESS
testing t1p73 in Haskell ... SUCCESS
testing t1p74 in Haskell ... SUCCESS
testing t1p87 in Haskell ... SUCCESS

```