### -*- Makefile -*-
###
### Author:
###   Gareth Smith <garethdanielsmith@gmail.com>
###

SOURCES= Main.aml

TARGETS= $(SOURCES:%.aml=%.alc)

INSTALLTO= `alice-config --alicetoolsdir`

.PHONY: all clean depend install

all: GtkBuilder.alc

GtkBuilder.alc: $(TARGETS)
	alicelink --include ./ ./Main -o $@

%.alc: %.aml
	alicec -c $< -o $@

clean:
	rm -f *.alc Makefile.depend

depend:
	alicedep $(SOURCES) > Makefile.depend

install: all
	install -d $(INSTALLTO)
	install GtkBuilder.alc $(INSTALLTO)
	install alicegtkbuilder `alice-config --alicebindir`

-include Makefile.depend
