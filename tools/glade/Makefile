### -*- Makefile -*-
###
### Author:
###   Guido Tack <tack@ps.uni-sb.de>
###
### Copyright:
###   Guido Tack, 2004
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

SOURCES= \
	Main.aml \
	CodeEmitter.aml \
	Parser.aml \
	GtkClasses.aml \
	GtkDefaults.aml

TARGETS= $(SOURCES:%.aml=%.alc)

INSTALLTO= `alice-config --alicetoolsdir`

.PHONY: clean all

all: Glade.alc

Glade.alc: $(TARGETS)
	alicelink --include ./ ./Main -o $@

%.alc: %.aml
	alicec -c $< -o $@

clean:
	rm -f $(TARGETS)
	rm -f Glade.alc

distclean: clean
	rm -f Makefile.depend

depend:
	alicedep $(SOURCES) > Makefile.depend

install: all
	install -d $(INSTALLTO)
	install Glade.alc $(INSTALLTO)
	install aliceglade `alice-config --alicebindir`

-include Makefile.depend
