## Makefile for UWYO COSC 4780/5010 Midterm - Java side
## Author: Finley McIlwaine

.PHONY : bnfc build parse compile test

bnfc/cpp/*.java: CPP.cf
	bnfc --java --makefile=bnfc.mk -p bnfc $^
	make -f bnfc.mk

*.class: *.java bnfc/cpp/*.java
	javac $^

bnfc: bnfc/cpp/*.java

build: *.class

parse: build bnfc
	@java bnfc.cpp.Test

compile: bnfc build
	@java Main

test: bnfc build
	@cd ../test && cabal -v0 run tester java

test_grad: bnfc build
	@cd ../test && cabal -v0 run tester java grad
