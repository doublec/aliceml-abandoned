TestAliceParser.alc:	Main.alc AliceParser.alc AliceLexer.alc
	alicelink -o TestAliceParser.alc ./Main --include ./

AliceLexer.aml:	AliceLexer.lex
	alicelex $< -o $@

AliceParser.aml:	AliceParser.y
	aliceyacc $< -o $@

%.alc:	%.aml
	alicec --no-warn-conventions --dump-phase-timings $< -o $@

Main.alc:	AliceParser.alc AliceLexer.alc AliceLexer-MLLex.alc

AliceLexer.alc:	AliceParser.alc

AliceLexer-MLLex.aml:	AliceLexer-MLLex.lex
	ml-lex $<
	echo -e 'import structure AliceParser from "AliceParser"\n' | cat - $<.sml > $@
	rm $<.sml

AliceLexer-MLLex.alc:	AliceParser.alc

clean:	
	rm -f *.alc AliceLexer.aml AliceParser.aml AliceLexer-MLLex.aml TestAliceParser.alc
	
