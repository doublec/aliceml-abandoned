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
	Highlight.aml HighlightString.aml \
	Buffer.aml \
	FileDialog.aml \
	SearchDialog.aml \
	Config.aml \
	History.aml \
	BufferHighlight.aml \
	BufferAnnotate.aml \
	AboutDialog.aml \
	Preferences.aml

all:	Main.alc \
	LinkedTextToplevel.alc LinkedGuiToplevel.alc LinkedCoreToplevel.alc \
	ToplevelIt.alc

LinkedCoreToplevel.alc: CoreToplevel.alc
	alicelink -z --include ./ ./$< -o $@

LinkedTextToplevel.alc: TextToplevel.alc
	alicelink -z --exclude ./CoreToplevel --include ./ ./$< -o $@

LinkedGuiToplevel.alc: GuiToplevel.alc
	alicelink -z --exclude ./CoreToplevel --include ./ ./$< -o $@

ToplevelIt.alc:
	(cd easteregg && make install)

%.alc: %.aml
	alicec $< -o $@

install: all
	install -d $(INSTALLTO)
	install Main.alc $(INSTALLTO)/Toplevel.alc
	install LinkedTextToplevel.alc $(INSTALLTO)/TextToplevel.alc
	install LinkedGuiToplevel.alc $(INSTALLTO)/GuiToplevel.alc
	install LinkedCoreToplevel.alc $(INSTALLTO)/CoreToplevel.alc
	install ToplevelStartup.aml $(INSTALLTO)/ToplevelStartup.aml
	install ToplevelIt.alc $(INSTALLTO)/ToplevelIt.alc
	install alice $(INSTALLBINTO)/alice

clean:
	rm -f *.alc
	(cd easteregg; make clean)

distclean: clean
	rm -f Makefile.depend

depend:
	alicedep $(SOURCES) > Makefile.depend

-include Makefile.depend
