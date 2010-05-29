CFLAGS = -Wall -Werror -g

test: testC testH

generated-dirs:
	mkdir -p generated/C
	mkdir -p generated/Haskell
	mkdir -p generated/Java

singleC:
	./test.pl -language=C $(SINGLE)

testC: generated-dirs
	./test.pl -language=C

testH: generated-dirs
	./test.pl -language=Haskell

testJ: generated-dirs
	./test.pl -language=Java

generated/C/sha256.c: sha256.stupid ../src/stupid.pl ../src/Stupid/C.pm
	./build-C.sh sha256

test_sha256.o: test_sha256.c generated/C/sha256.c

test_sha256: test_sha256.o
	$(CC) $(CFLAGS) -o test_sha256 test_sha256.o
