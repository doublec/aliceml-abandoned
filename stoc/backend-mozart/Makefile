SOURCES = Frontend.oz Builtins.oz CodeGen.oz Assembler.oz

.PHONY: all clean

%.ozf: %.oz
	ozc $(OZCFLAGS) -c $< -o $@

all: $(SOURCES:%.oz=%.ozf)

CodeGen.ozf: CodeGen.oz CodeEmitter.oz CodeStore.oz

clean:
	-rm -f *.ozf
