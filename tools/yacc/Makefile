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


.PHONY: all with-smlnj clean distclean

all:	with-smlnj aliceyacclib

aliceyacclib:
	(cd bootstrap; make)


with-smlnj: jacke-image

jacke-image: ${FILES}
	sml <wrap-smlnj.sml

clean:
	rm -f *.u[oi]
	rm -f *.ozf
	rm -rf CM
	rm -f *~


distclean:	clean
	rm -f *.grm.*
	rm -f *.lex.*
	rm -rf *-image.*


%.grm.sig: %.grm
	ml-yacc $<

%.grm.sml: %.grm
	ml-yacc $<

%.lex.sml: %.lex
	ml-lex $<
