FIXPARSER=../../bootstrap/fixparser.awk

FILES = AbsSyn.aml \
	Collect.aml \
	Extract.aml \
	Hose.aml \
	Lexer.aml \
	Output.aml \
	Parse.aml \
	Table.aml \
	Container.aml \
	ABS_SYN-sig.aml \
	COLLECT-sig.aml \
	EXTRACT-sig.aml \
	HOSE-sig.aml \
	OUTPUT-sig.aml \
	PARSE-sig.aml \
	TABLE-sig.aml

LINKED_HOSE = AliceLex-Linked.alc

GENERATED_FILES = HoseParser.aml HOSE_PARSER-sig.aml HoseLexer.aml

INSTALL_BIN_TO= `alice-config --alicebindir`
INSTALL_TOOL_TO=`alice-config --alicetoolsdir`
INSTALL_LIB_TO=`alice-config --alicelibdir`/lex


.PHONY:	all clean distclean depend install

all:	$(LINKED_HOSE) LEXER-sig.alc Lexer.alc

install: $(LINKED_HOSE) LEXER-sig.alc Lexer.alc
	install -d $(INSTALL_TOOL_TO)
	install $(LINKED_HOSE) $(INSTALL_TOOL_TO)
	install -d $(INSTALL_BIN_TO)
	install alicelex $(INSTALL_BIN_TO)
	install -d $(INSTALL_LIB_TO)
	install LEXER-sig.alc Lexer.alc $(INSTALL_LIB_TO)


depend:	$(GENERATED_FILES)
	alicedep $(FILES) $(GENERATED_FILES) > Makefile.depend

$(LINKED_HOSE): $(FILES:%.aml=%.alc) $(GENERATED_FILES:%.aml=%.alc)
	alicelink -z -v ./Hose --include ./ -o $(LINKED_HOSE)

clean:
	rm -f *.u[oi]
	rm -f *.alc

distclean:	clean
	rm -f *.grm.*
	rm -f *.lex.*
	rm -f $(GENERATED_FILES)

%.alc:	%.aml
	alicec -z --no-warn-conventions $< -o $@

HoseParser.aml HOSE_PARSER-sig.aml:	hose.grm
	ml-yacc hose.grm
	printf 'import signature PARSER_DATA from "x-alice:/lib/ml-yacc/base-sig"\n\n' > HOSE_PARSER-sig.aml
	awk -f $(FIXPARSER) < hose.grm.sig  >> HOSE_PARSER-sig.aml
	cat HoseParser.aml.header hose.grm.sml > HoseParser.aml
	rm hose.grm.sml
	rm hose.grm.sig


HoseLexer.aml: hose.lex  
	ml-lex hose.lex
	printf 'import signature Parser_TOKENS signature Parser_LRVALS from "HOSE_PARSER-sig"\nimport structure AbsSyn from "AbsSyn"\n\n' > HoseLexer.aml.tmp
	cat hose.lex.sml >> HoseLexer.aml.tmp
	sed -e 's/Unsafe.//g' HoseLexer.aml.tmp > HoseLexer.aml
	rm HoseLexer.aml.tmp
	rm hose.lex.sml

-include Makefile.depend
