SIGS	= ELAB ENV EVAL LEXER PARSER TYPE VALUE
STRUCTS	= Elab Env Eval Lexer Main Parser Syntax Token Type Value
FILES	= $(SIGS:%=%-sig.aml) $(STRUCTS:%=%.aml)

all: $(FILES:%.aml=%.ozf)

depend:
	alicedep $(FILES) >Makefile.depend

-include Makefile.depend

%.ozf: %.aml
	alicec -c $< -o $@
clean:
	rm -rf *.ozf

distclean: clean
	rm -rf Makefile.depend
