INSTALLTO= `alice-config --alicetoolsdir`
INSTALLBINTO = `alice-config --alicebindir`

SOURCES = AliceIdeGUI.aml \
	AliceIdeMain.aml \
	OutputWriters.aml \
	EventHandler.aml \
	AliceIdeGUISignals.aml \
	AliceIde.aml \
	EVALUATOR-sig.aml \
	Evaluator.aml \
	Use.aml \
	Highlight.aml \
	Buffer.aml \
	FileDialog.aml

all: AIDE.alc

AIDE.alc: $(SOURCES:%.aml=%.alc)
	alicelink -z --include ./ ./AliceIdeMain -o $@

%.alc: %.aml
	alicec $< -o $@

install: all
	install -d $(INSTALLTO)
	install AIDE.alc $(INSTALLTO)/AIDE.alc
	install aide $(INSTALLBINTO)/aide

clean:
	rm -f $(SOURCES:%.aml=%.alc)
	rm -f AIDE.alc

depend:
	alicedep $(SOURCES) > Makefile.depend

-include Makefile.depend
