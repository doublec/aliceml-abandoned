###
### Author:
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
###
### Copyright:
###   Leif Kornstaedt, 1999-2001
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

SRCDIR=..

SML := $(shell pwd)/smla
PLATFORM = x86-linux

OZC = ozc
OZCFLAGS =
OZL = ozl
OZLFLAGS =

FUNCTORS = \
	Builtins.ozf Prebound.ozf CodeStore.ozf CodeGen.ozf Assembler.ozf \
	Main.ozf

%.ozf: $(SRCDIR)/stoc/backend-mozart/%.oz
	$(OZC) $(OZCFLAGS) -c $< -o $@

.PHONY: all clean

all: stodep.$(PLATFORM) stoc-mozart.exe stoc-mozart.$(PLATFORM)

stoc-mozart.exe: $(FUNCTORS)
	$(OZL) $(OZLFLAGS) -x Main.ozf -o $@

CodeStore.ozf: $(SRCDIR)/stoc/backend-mozart/CodeEmitter.oz

stodep.$(PLATFORM):
	-rm -f $@
	echo 'use "make-stodep.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

stoc-mozart.$(PLATFORM):
	-rm -f $@
	echo 'use "make-stoc-mozart.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

stot-mozart.$(PLATFORM):
	-rm -f $@
	echo 'use "make-stot-mozart.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

stoc-com+.$(PLATFORM):
	-rm -f $@
	echo 'use "make-stoc-com+.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

clean:
	-rm -f *.ozf

veryclean: clean
	-rm -f stoc-mozart.exe
	-rm -f stodep.* stoc-mozart.* stot-mozart.* stoc-com+.*

distclean: veryclean
