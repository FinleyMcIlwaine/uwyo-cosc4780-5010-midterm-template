## Makefile for UWYO COSC 4780/5010 Midterm - Haskell side
## Author: Finley McIlwaine

.PHONY : bnfc parse compile test test_grad

bnfc/*.hs: CPP.cf
	bnfc -m -o bnfc $^
	cd bnfc && make

bnfc: bnfc/*.hs

parse: bnfc
	@./bnfc/TestCPP

compile: bnfc
	@cabal run -v0 midterm

test: bnfc
	@cd ../test && cabal -v0 run tester haskell

test_grad: bnfc
	@cd ../test && cabal -v0 run tester haskell grad
