.SUFFIXES: .oz .ozf

FUNCTORS = Frontend.ozf Builtins.ozf Prebound.ozf \
	CodeStore.ozf CodeGen.ozf Assembler.ozf

.oz.ozf:
	ozc $(OZCFLAGS) -c $< -o $@

all: $(FUNCTORS)

CodeStore.ozf: CodeEmitter.oz

clean:
	-rm -f *.ozf

.PHONY: all clean
