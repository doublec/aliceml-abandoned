NATIVES = SelectFD SelectFS SelectUnion SelectInter SeqUnion SelectThe
PLATFORM = $(shell oztool platform)

.PHONY:	all clean

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

clean:
	-rm -f *.o *.so-* *.ozf *.ozm runme
	-rm -f *.aux *.log *.dvi *.ps

Tag.ozf:	
Doc.ozf:	Tag.ozf
Meaning.ozf:	Tag.ozf
Lex.ozf:	Tag.ozf
Parse.ozf:	Tag.ozf Meaning.ozf Lex.ozf
Main.ozf:	Parse.ozf Smurf.asig Doc.ozf Meaning.ozf
