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
	PickleMinimizer.aml

TARGETS= $(SOURCES:%.aml=%.alc)

INSTALLTO= `alice-config --alicetoolsdir`

.PHONY: clean all

all: PMinimizer.alc

PMinimizer.alc: $(TARGETS)
	alicelink --include ./ ./Main -o $@

%.alc: %.aml
	alicec -c $< -o $@

clean:
	rm -f $(TARGETS)
	rm -f PMinimizer.alc

distclean: clean
	rm -f Makefile.depend

depend:
	alicedep $(SOURCES) > Makefile.depend

install: all
	install -d $(INSTALLTO)
	install PMinimizer.alc $(INSTALLTO)
	install pminimizer `alice-config --alicebindir`

-include Makefile.depend
