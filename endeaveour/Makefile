HAPPY=happy
HAPPY_OPTS=--info=grammar.info -g -a -d

ALEX=alex
ALEX_OPTS=

GHC=ghc
GHC_OPTS=

RM=rm -f


SRCS=$(wildcard *.hs)
PRGS=${SRCS:.hs=}
OBJS=$(SRCS:.hs=.o)
HIS=$(SRCS:.hs=.hi)

all: $(OBJS)


%.o : %.hs
	$(GHC) $(GHC_OPTS) $< 

dis: 
	llvm-dis-mp-3.2 bitcode.bc

ll:
	cat bitcode.ll

clean:
	$(RM) $(PRGS) $(OBJS) $(HIS)
	$(RM) *.bc *.ll

.PHONY: clean all
