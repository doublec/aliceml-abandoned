INSTALLTO= `alice-config --alicetoolsdir`
INSTALLBINTO = `alice-config --alicebindir`

SOURCES = \
	Main.aml \
	CORE_TOPLEVEL-sig.aml CoreToplevel.aml \
	TOPLEVEL-sig.aml \
	TextToplevel.aml \
	GuiToplevel.aml \
	Gui.aml \
	GuiMain.aml \
	OutputWriters.aml \
	EventHandler.aml \
	GuiSignals.aml \
	EVALUATOR-sig.aml \
	Evaluator.aml \
	Use.aml \
	Highlight.aml \
	Buffer.aml \
	FileDialog.aml \
	SearchDialog.aml \
	Config.aml \
	BufferHighlight.aml

all: Main.alc
#LinkedTextToplevel.alc LinkedGuiToplevel.alc LinkedCoreToplevel.alc
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
