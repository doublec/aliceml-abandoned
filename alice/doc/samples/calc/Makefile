Calc.alc:	Main.alc CalcParser.alc CalcLexer.alc
	alicelink -o Calc.alc ./Main --include ./

CalcLexer.aml:	CalcLexer.lex
	alicelex $< -o $@

CalcParser.aml:	CalcParser.y
	aliceyacc $< -o $@

%.alc:	%.aml
	alicec --no-warn-conventions $< -o $@

Main.alc:	CalcParser.alc CalcLexer.alc

CalcLexer.alc:	CalcParser.alc

clean:	
	rm -f *.alc CalcLexer.aml CalcParser.aml
	
