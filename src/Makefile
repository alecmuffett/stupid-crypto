grammar.pm: grammar.y
	yapp -v grammar.y

test: grammar.pm stupid.pl
	./stupid.pl t1.stupid
