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
LINKEDNAME= AliceLex.alc
LINKFILES= $(FILES:%.aml=--include %)
ROOTCOMPONENT= Hose
INSTALLTO= `alice-config --alicelibdir`/tools

.PHONY: all compile_all clean distclean copy


all: $(FILES:%.aml=%.alc)
	@echo "Schnitzel time"
	
	
$(LINKEDNAME):	compile_all
	alicelink -v $(LINKFILES) $(ROOTCOMPONENT) -o $(LINKEDNAME)
	

compile_all: copy $(FILES:%.aml=%.alc)
	
copy:
	cp ../hose-image* .
	cp ../../yacc/jacke-image* .

%.alc: %.aml
	$(ALICEC) $< -o $@

$(H-FILE).aml: $(H-FILE)
	$(HOSE) $< -o $@

$(J-FILE).aml: $(J-FILE)
	$(JACKE) $<

clean:
	$(RM) *~


install: $(LINKEDNAME)
	mkdir -p -m 755 $(INSTALLTO)
	install -c -m 444 $(LINKEDNAME) $(INSTALLTO)/AliceLex.alc


distclean: clean
	$(RM) $(FILES:%.aml=%.alc)
	$(RM) jacke.jacke.aml
	$(RM) hose.hose.aml
	$(RM) hose-image.*
	$(RM) jacke-image.*
