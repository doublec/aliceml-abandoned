.PHONY:	all clean

all:	Main.ozf Smurf.ozf

%.ozf:	%.aml
	alicec -c $< -o $@

%.ozf:	%.oz
	ozc -c $< -o $@

clean:
	rm *.ozf *.ozm

Tag.ozf:	
Doc.ozf:	Tag.ozf
Meaning.ozf:	Tag.ozf
Lex.ozf:	Tag.ozf
Parse.ozf:	Tag.ozf Meaning.ozf Lex.ozf
Main.ozf:	Parse.ozf Smurf.asig Doc.ozf Meaning.ozf
