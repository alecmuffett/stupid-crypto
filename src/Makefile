grammar2.pm: grammar2.y
	yapp -v grammar2.y

grammar.pm: grammar.y
	yapp -v grammar.y

test: grammar.pm
	cd ../test && make test

testC: grammar.pm stupid.pl
	./stupid.pl --language=C ../test/t1.stupid

testH: grammar.pm stupid.pl
	./stupid.pl --language=Haskell ../test/t1.stupid

tarball:
	tar cvfz stupid.tgz CHANGES Makefile grammar.y stupid.pl Stupid/C.pm t1.stupid

push:
	cd .. && make push
