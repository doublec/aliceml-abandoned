NATIVES = SelectFD SelectFS SelectUnion SelectInter SeqUnion SelectThe
PLATFORM = $(shell oztool platform)

.PHONY:	all clean submission

all:	$(NATIVES:%=%.so-$(PLATFORM)) runme

runme: Linked.ozf
	ozl -z9 -x $< -o $@

Linked.ozf:	Main.ozf Select.ozf Smurf.ozf
	alicelink -v $< -o $@ --include=x-alice

%.o: %.cc
	oztool c++ -O3 -c $< -o $@

%.so-$(PLATFORM): %.o
	oztool ld -o $@ $<

%.ozf:	%.aml
	alicec -c $< -o $@

%.ozf:	%.oz
	ozc -c $< -o $@

%.ps:	%.tex
	latex $<
	dvips $(<:%.tex=%.dvi) -o $@

submission:	runme readme.ps README
	mkdir helikopter
	mkdir helikopter/source
	cp *.aml Smurf.oz *.asig Makefile helikopter/source/
	cp buildme runme *.so-linux* README readme.ps helikopter/
	tar -czf helikopter.tar.gz helikopter
	rm -r helikopter
	

clean:
	-rm -f *.o *.so-* *.ozf *.ozm runme
	-rm -f *.aux *.log *.dvi *.ps
	-rm -rf helikopter

Tag.ozf:	
Doc.ozf:	Tag.ozf
Meaning.ozf:	Tag.ozf
Lex.ozf:	Tag.ozf
Parse.ozf:	Tag.ozf Meaning.ozf Lex.ozf
Main.ozf:	Parse.ozf Smurf.asig Doc.ozf Meaning.ozf
