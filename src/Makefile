grammar.pm: grammar.y
	yapp -v grammar.y

test:
	cd ../test && make test

testC: grammar.pm stupid.pl
	./stupid.pl --language=C t1.stupid

testH: grammar.pm stupid.pl
	./stupid.pl --language=Haskell t1.stupid

tarball:
	tar cvfz stupid.tgz CHANGES Makefile grammar.y stupid.pl Stupid/C.pm t1.stupid

push:
	hg push https://stupid-crypto.googlecode.com/hg/
