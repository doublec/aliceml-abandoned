INSTALLTO= `alice-config --alicelibdir`/tools/toplevel

SOURCES = TOPLEVEL-sig.aml Toplevel.aml

all: $(SOURCES:%.aml=%.alc)

%.alc: %.aml
	alicec -z $< -o $@

install: all
	install -d $(INSTALLTO)
	install $(SOURCES:%.aml=%.alc) $(INSTALLTO)
	install ToplevelStartup.aml $(INSTALLTO)

clean:
	rm -f *.alc

depend:
	alicedep $(SOURCES) > Makefile.depend

-include Makefile.depend
