TestAliceParser.alc:	Main.alc AliceParser.alc AliceLexer.alc
	alicelink -o TestAliceParser.alc ./Main --include ./

AliceLexer.aml:	AliceLexer.lex
	alicelex $< -o $@

AliceParser.aml:	AliceParser.y
	aliceyacc $< -o $@

%.alc:	%.aml
	alicec --no-warn-conventions $< -o $@

Main.alc:	AliceParser.alc AliceLexer.alc

AliceLexer.alc:	AliceParser.alc

clean:	
	rm -f *.alc AliceLexer.aml AliceParser.aml TestAliceParser.alc
	
