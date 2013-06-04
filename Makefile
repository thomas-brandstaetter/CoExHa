
HI=hi
OBJ=obj
SRC=src
BUILD=build
DEFS=$(SRC)/defs

HAPPY=happy
#HAPPY_OPTS=--info=grammar.info -g -a -d
HAPPY_OPTS=

ALEX=alex
ALEX_OPTS=

GHC=ghc
GHC_OPTS=


all: build


build: generate
	mkdir -p $(OBJ)
	mkdir -p $(HI)
	mkdir -p $(BUILD)
	$(GHC) $(GHC_OPTS) -odir $(OBJ) -hidir $(HI) -o build/myce $(SRC)/Main.hs $(SRC)/Scanner.hs $(SRC)/Parser.hs $(SRC)/CodeGen.hs $(SRC)/TypeNames.hs 
	chmod +x $(BUILD)/myce


generate: $(SRC)/Scanner.hs $(SRC)/Parser.hs

$(SRC)/Scanner.hs: $(DEFS)/scanner.x
	$(ALEX) $(ALEX_OPTS) $(DEFS)/scanner.x
	mv $(DEFS)/scanner.hs $(SRC)/Scanner.hs

$(SRC)/Parser.hs: $(DEFS)/parser.y
	$(HAPPY) $(HAPPY_OPTS)  $(DEFS)/parser.y
	mv $(DEFS)/parser.hs $(SRC)/Parser.hs

dis: bitcode.ir
	llvm-dis-mp-3.2 -o bitcode.dis bitcode.ir

clean: 
	cd $(SRC); rm -rf Scanner.hs Parser.hs
	rm -rf $(OBJ) $(HI) $(BUILD)

.PHONY: clean
