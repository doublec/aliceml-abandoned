FILES =	Tag Doc Meaning Lex Parse Smurf Main


.PHONY:	all

all:	${FILES:%=%.ozf}

%.ozf:	%.aml
	alicec -c $< -o $@

%.ozf:	%.oz
	ozc -c $< -o $@

clean:
	rm *.ozf *.ozm

Main.ozf: Smurf.asig
