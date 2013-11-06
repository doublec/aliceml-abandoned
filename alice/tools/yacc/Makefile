FIXPARSER=../../bootstrap/fixparser.awk

AML_FILES = AbsSyn.aml Core.aml CoreUtils.aml ErrorMsg.aml Grammar.aml Graph.aml Lalr.aml Look.aml LrParser.aml Main.aml MAIN-sig.aml MkLrTable.aml MkPrStruct.aml NormalForm.aml Output.aml Parse.aml Set.aml Shrink.aml Sigs.aml Translate.aml Utils.aml UTILS-sig.aml Verbose.aml

LINKED_YACC = AliceYacc-Linked.alc

GENERATED_AML_FILES = JackeParser.aml JACKE_PARSER-sig.aml Lexer.aml

ALICELIB=LrParser.aml
INSTALL_LIB_TO= `alice-config --alicelibdir`/yacc
INSTALL_BIN_TO= `alice-config --alicebindir`
INSTALL_TOOL_TO=`alice-config --alicetoolsdir`

.PHONY: install all clean distclean aliceyacclib depend

all:	$(LINKED_YACC) aliceyacclib

aliceyacclib: $(ALICELIB:%.aml=%.alc)

depend: $(GENERATED_AML_FILES)
	alicedep $(AML_FILES) $(GENERATED_AML_FILES) > Makefile.depend


install: aliceyacclib $(LINKED_YACC) 
	# install library
	install -d $(INSTALL_LIB_TO)
	install $(ALICELIB:%.aml=%.alc) $(INSTALL_LIB_TO)
	# install tool
	install -d $(INSTALL_TOOL_TO)
	install $(LINKED_YACC) $(INSTALL_TOOL_TO)
	# install binary
	install -d $(INSTALL_BIN_TO)
	install aliceyacc $(INSTALL_BIN_TO)

$(LINKED_YACC):	$(AML_FILES:%.aml=%.alc) $(GENERATED_AML_FILES:%.aml=%.alc)
	alicelink -z -v ./Main --include ./ -o $(LINKED_YACC)

clean:
	rm -f *.u[oi]
	rm -f *.alc
	rm -rf CM
	rm -f *~


distclean:	clean
	rm -f *.grm.*
	rm -f *.lex.*
	rm -rf *-image.*
	rm -f $(GENERATED_AML_FILES)

%.alc:	%.aml
	alicec -z --no-warn-conventions $< -o $@

JackeParser.aml JACKE_PARSER-sig.aml:	jacke.grm
	ml-yacc jacke.grm
	printf 'import signature PARSER_DATA from "Sigs"\n\n' > JACKE_PARSER-sig.aml
	awk -f $(FIXPARSER) < jacke.grm.sig  >> JACKE_PARSER-sig.aml
	cat JackeParser.aml.header jacke.grm.sml > JackeParser.aml
	rm jacke.grm.sml
	rm jacke.grm.sig


Lexer.aml: Jacke.lex  
	ml-lex Jacke.lex
	printf 'import signature Parser_TOKENS signature Parser_LRVALS from "JACKE_PARSER-sig"\nimport structure ErrorMsg from "ErrorMsg"\n\n' > Lexer.aml.tmp
	cat Jacke.lex.sml >> Lexer.aml.tmp
	sed -e 's/Unsafe.//g' Lexer.aml.tmp > Lexer.aml
	rm Lexer.aml.tmp
	rm Jacke.lex.sml


-include Makefile.depend
