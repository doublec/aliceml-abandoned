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

.PHONY: with-smlnj clean distclean

with-smlnj: hose-image.x86-linux

hose-image.x86-linux: ${FILES}
	sml <wrap-smlnj.sml

clean:
	rm -f *.u[oi]
	rm -f *.ozf
	rm -rf CM
	rm -f *~


distclean:	clean
	rm -rf *-image.*


%.grm.sig: %.grm
	ml-yacc $<

%.grm.sml: %.grm
	ml-yacc $<

%.lex.sml: %.lex
	ml-lex $<
