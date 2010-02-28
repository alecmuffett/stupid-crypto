
progs: stupid-haskell-sha256sum stupid-haskell-rot13 Rot13cli.class

clean:
	rm -f *.o sha256.hs *.hi stupid-haskell-sha256sum

sha256.hs: ../generated/sha256.hs
	cp ../generated/sha256.hs .

rot13.hs: ../generated/rot13.hs
	cp ../generated/rot13.hs .

stupid-haskell-sha256sum: sha256.hs sha256cli.hs
	ghc -i../../lib:. --make sha256.hs sha256cli.hs -o stupid-haskell-sha256sum

stupid-haskell-rot13: rot13.hs rot13cli.hs
	ghc -i../../lib:. --make rot13.hs rot13cli.hs -o stupid-haskell-rot13

Rot13cli.class: rot13.java
	javac Rot13cli.java ../generated/rot13.java ../../lib/Stupid/Mutable.java

