grammar.pm: grammar.y
	yapp -v grammar.y

test:
	cd ../test && make test

testC: grammar.pm stupid.pl
	./stupid.pl --language=C ../test/t1.stupid

testH: grammar.pm stupid.pl
	./stupid.pl --language=Haskell ../test/t1.stupid

tarball:
	tar cvfz stupid.tgz CHANGES Makefile grammar.y stupid.pl Stupid/C.pm t1.stupid
