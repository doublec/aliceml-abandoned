INSTALLTO= `alice-config --alicelibdir`/tools

SOURCES = TOPLEVEL-sig.aml Toplevel.aml

all: LinkedToplevel.alc

LinkedToplevel.alc: $(SOURCES:%.aml=%.alc)
	alicelink -z -v ./Toplevel -o $@

%.alc: %.aml
	alicec -z $< -o $@

install: all
	install -d $(INSTALLTO)
	install LinkedToplevel.alc $(INSTALLTO)/Toplevel.alc
	install ToplevelStartup.aml $(INSTALLTO)

clean:
	rm -f *.alc

distclean: clean

depend:
	alicedep $(SOURCES) > Makefile.depend

-include Makefile.depend
