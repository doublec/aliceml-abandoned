SML = sml-cm
PLATFORM = x86-linux

.PHONY: stoc-frontend.$(PLATFORM)

all: stoc-frontend.$(PLATFORM)

stoc-frontend.$(PLATFORM):
	$(SML) < Executable.sml

clean:
	-rm -rf CM

veryclean: clean
	-rm -rf Lexer.lex.* Parser.grm.* stoc-frontend.*
