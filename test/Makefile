CFLAGS = -Wall -Werror -g

test: testC testH

generated:
	mkdir generated

testC: generated
	./test.pl -language=C

testH: generated
	./test.sh Haskell

testJ: generated
	./test.sh Java

generated/sha256.c: sha256.stupid ../src/stupid.pl ../src/Stupid/C.pm
	./build-C.sh sha256

test_sha256.o: test_sha256.c generated/sha256.c

test_sha256: test_sha256.o
	$(CC) $(CFLAGS) -o test_sha256 test_sha256.o
