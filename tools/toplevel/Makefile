SOURCES = AliceIdeGUI.aml \
	AliceIdeMain.aml \
	OutputWriters.aml \
	EventHandler.aml \
	AliceIdeGUISignals.aml \
	AliceIde.aml \
	EVALUATOR-sig.aml \
	Evaluator.aml \
	Highlight.aml

all: $(SOURCES:%.aml=%.alc)

%.alc: %.aml
	alicec $< -o $@

depend:
	alicedep $(SOURCES) > Makefile.depend

-include Makefile.depend
