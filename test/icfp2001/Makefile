FILES =	Tag Doc Meaning Lex Parse Main


.PHONY:	all

all:	${FILES:%=%.ozf}

%.ozf:	%.aml
	alicec -c $< -o $@

clean:
	rm *.ozf *.ozm
