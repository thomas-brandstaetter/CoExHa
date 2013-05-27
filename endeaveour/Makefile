HAPPY=happy
HAPPY_OPTS=--info=grammar.info -g -a -d

ALEX=alex
ALEX_OPTS=

GHC=ghc
GHC_OPTS=


all: first_step addMul

first_step: first_step.hs
	$(GHC) $(GHC_OPTS) -o first_step first_step.hs


addMul: addMul.hs
	$(GHC) $(GHC_OPTS) -o addMul addMul.hs


clean:
	rm addMul first_step  addMul.{hi,o} first_step.{hi,o} *.bc

.PHONY: clean
