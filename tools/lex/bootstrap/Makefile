H-FILE = hose.hose

J-FILE = jacke.jacke

FILES =	container.aml \
	ABS_SYN-sig.aml \
	AbsSyn.aml \
	LEXER-sig.aml \
	Lexer.aml \
	../../yacc/bootstrap/LrTable.aml \
	../../yacc/bootstrap/LrParser.aml \
	JACKE-sig.aml \
	$(J-FILE:%=%.aml) \
	HOSE_LEXER-sig.aml \
	$(H-FILE:%=%.aml) \
	PARSE-sig.aml \
	Parse.aml \
	COLLECT-sig.aml \
	Collect.aml \
	EXTRACT-sig.aml \
	Extract.aml \
	TABLE-sig.aml \
	Table.aml \
	OUTPUT-sig.aml \
	Output.aml \
	HOSE-sig.aml \
	Hose.aml

RM = rm -f
ALICEC = alicec --no-warn-conventions
HOSE = ../hose
JACKE = ../../yacc/jacke

.PHONY: all clean distclean copy


all: copy $(FILES:%.aml=%.ozf)

copy:
	cp ../hose-image* .
	cp ../../yacc/jacke-image* .

%.ozf: %.aml
	$(ALICEC) $< -o $@

$(H-FILE).aml: $(H-FILE)
	$(HOSE) $< -o $@

$(J-FILE).aml: $(J-FILE)
	$(JACKE) $<

clean:
	$(RM) *~


distclean: clean
	$(RM) $(FILES:%.aml=%.ozf)
	$(RM) jacke.jacke.aml
	$(RM) hose.hose.aml
	$(RM) hose-image.*
	$(RM) jacke-image.*
