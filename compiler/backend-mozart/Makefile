.SUFFIXES: .oz .ozf

FUNCTORS = Builtins.ozf Prebound.ozf \
	CodeStore.ozf CodeGen.ozf Assembler.ozf Main.ozf

#OZCFLAGS = -p

.oz.ozf:
	ozc $(OZCFLAGS) -c $< -o $@

all: stoc-mozart.exe

stoc-mozart.exe: $(FUNCTORS)
	ozl $(OZLFLAGS) -x Main.ozf -o stoc-mozart.exe

CodeStore.ozf: CodeEmitter.oz

clean:
	-rm -f *.ozf

veryclean: clean
	-rm -f stoc-mozart.exe

.PHONY: all clean
