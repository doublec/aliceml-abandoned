###
### Author:
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
###
### Copyright:
###   Leif Kornstaedt, 1999-2003
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

SRCDIR=..

SML := $(shell pwd)/smla
PLATFORM := $(shell ./platform.sh smlnj)

OZC = ozc
OZCFLAGS =
OZL = ozl
OZLFLAGS =

FUNCTORS = Prebound.ozf CodeGen.ozf Main.ozf

%.ozf: $(SRCDIR)/compiler/backend-mozart/%.oz
	$(OZC) $(OZCFLAGS) -c $< -o $@

.PHONY: all clean

all: alicedep.$(PLATFORM) alicec-mozart.exe \
	alicec-mozart.$(PLATFORM) alicec-seam.$(PLATFORM)

alicec-mozart.exe: $(FUNCTORS)
	$(OZL) $(OZLFLAGS) -x Main.ozf -o $@

CodeStore.ozf: $(SRCDIR)/compiler/backend-mozart/CodeEmitter.oz

alicedep.$(PLATFORM):
	-rm -f $@
	echo 'use "make-alicedep.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

alicec-mozart.$(PLATFORM):
	-rm -f $@
	echo 'use "make-alicec-mozart.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

alice-mozart.$(PLATFORM):
	-rm -f $@
	echo 'use "make-alice-mozart.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

alicec-dotnet.$(PLATFORM):
	-rm -f $@
	echo 'use "make-alicec-dotnet.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

alicec-seam.$(PLATFORM):
	-rm -f $@
	echo 'use "make-alicec-seam.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

clean:
	-rm -f *.ozf

veryclean: clean
	-rm -f alicec-mozart.exe
	-rm -f alicedep.*
	-rm -f alicec-mozart.* alice-mozart.*
	-rm -f alicec-dotnet.*
	-rm -f alicec-seam.*

distclean: veryclean
