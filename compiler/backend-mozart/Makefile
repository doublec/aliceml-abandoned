SOURCES = Frontend.oz Builtins.oz Prebound.oz \
	CodeStore.oz CodeGen.oz Assembler.oz

.PHONY: all clean

%.ozf: %.oz
	ozc $(OZCFLAGS) -c $< -o $@

all: $(SOURCES:%.oz=%.ozf)

CodeStore.ozf: CodeEmitter.oz

clean:
	-rm -f *.ozf
