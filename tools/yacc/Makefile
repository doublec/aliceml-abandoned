FILES =	jacke.lex.sml \
	jacke.grm.sml \
	jacke.grm.sig \
	ErrorMsg.sml \
	Set.sml \
	AbsSyn.sml \
	NormalForm.sml \
	Translate.sml \
	parse.sml \
	utils.sig \
	utils.sml \
	sigs.sml \
	grammar.sml \
	core.sml \
	coreutils.sml \
	graph.sml \
	look.sml \
	lalr.sml \
	mklrtable.sml \
	shrink.sml \
	mkprstruct.sml \
	verbose.sml \
	output.sml \
	sources.cm \
	Main.sml \
	MAIN-sig.sml


ALICELIB=LrTable.aml LrParser.aml
INSTALL_LIB_TO= `alice-config --alicelibdir`/aliceyacc
INSTALL_BIN_TO= `alice-config --alicebindir`

.PHONY: all with-smlnj clean distclean aliceyacclib

all:	with-smlnj aliceyacclib

LrTable.aml: LrTable.sml
	cp LrTable.sml LrTable.aml

LrParser.aml: LrParser.sml
	echo -e 'import signature LR_TABLE structure LrTable from "LrTable"\n' > LrParser.aml
	cat LrParser.sml >> LrParser.aml


aliceyacclib: $(ALICELIB:%.aml=%.alc)

depend:
	alicedep $(SOURCES) > Makefile.depend


install: aliceyacclib jacke-image
	# install library
	install -d $(INSTALL_LIB_TO)
	install $(ALICELIB:%.aml=%.alc) $(INSTALL_LIB_TO)
	# install binary
	install -d $(INSTALL_BIN_TO)
	install aliceyacc jacke-image* $(INSTALL_BIN_TO)
	
with-smlnj: jacke-image

jacke-image: ${FILES}
	sml <wrap-smlnj.sml

clean:
	rm -f *.u[oi]
	rm -f *.alc
	rm -rf CM
	rm -f *~


distclean:	clean
	rm -f *.grm.*
	rm -f *.lex.*
	rm -rf *-image.*

%.alc:	%.aml
	alicec $< -o $@

%.grm.sig: %.grm
	ml-yacc $<

%.grm.sml: %.grm
	ml-yacc $<

%.lex.sml: %.lex
	ml-lex $<


-include Makefile.depend
