FILES = AbsSyn.sml \
	Collect.sml \
	Extract.sml \
	Hose.sml \
	Lexer.sml \
	Output.sml \
	Parse.sml \
	Table.sml \
	container.sml \
	hose.grm.sig \
	hose.grm.sml \
	hose.lex.sml \
	sources.cm \
	wrap-smlnj.sml \
	ABS_SYN-sig.sml \
	COLLECT-sig.sml \
	EXTRACT-sig.sml \
	HOSE-sig.sml \
	OUTPUT-sig.sml \
	PARSE-sig.sml \
	TABLE-sig.sml

.PHONY: with-smlnj bootstrap clean distclean

with-smlnj: hose-image

bootstrap: with-smlnj
	make -C bootstrap

hose-image: ${FILES}
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
