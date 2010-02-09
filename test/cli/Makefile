
progs: stupid-haskell-sha256sum

clean:
	rm -f *.o sha256.hs *.hi stupid-haskell-sha256sum

sha256.hs: ../generated/sha256.hs
	cp ../generated/sha256.hs .

stupid-haskell-sha256sum: sha256.hs sha256cli.hs
	ghc -i../../lib:. --make sha256.hs sha256cli.hs -o stupid-haskell-sha256sum

