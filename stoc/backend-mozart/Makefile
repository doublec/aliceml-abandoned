.SUFFIXES: .oz .ozf

FUNCTORS = Frontend.ozf Builtins.ozf Prebound.ozf \
	CodeStore.ozf CodeGen.ozf Assembler.ozf Main.ozf

.oz.ozf:
	ozc $(OZCFLAGS) -c $< -o $@

all: stoc-mozart

stoc-mozart: $(FUNCTORS)
	ozl $(OZLFLAGS) -x Main.ozf -o stoc-mozart

CodeStore.ozf: CodeEmitter.oz

clean:
	-rm -f *.ozf

veryclean: clean
	-rm -f stoc-mozart

.PHONY: all clean
