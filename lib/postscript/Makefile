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

INSTALLTO= `alice-config --alicelibdir`/postscript

.PHONY: all install clean

all: PS.stc

install: all
	mkdir -p $(INSTALLTO)
	install -c -m444 PS.stc $(INSTALLTO)
	install -c -m444 prolog.ps $(INSTALLTO)

clean:
	rm PS.stc

%.stc: %.aml
	alicec -c $< -o $@
