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
ifeq ("$(PLATFORM)", "")
  $(error Unknown platform)
endif

OZC = ozc
OZCFLAGS =
OZL = ozl
OZLFLAGS =
M4 = m4

OZBANNER := $(shell $(OZC) -v 2>&1|grep 1.2.\[0-5\])
ifeq ("$(OZBANNER)", "")
    PICKLE_VERSION = 3
else
    PICKLE_VERSION = 2
endif

FUNCTORS = Prebound.ozf CodeGen.ozf Main.ozf

.PHONY: mozart seam clean

mozart: alicedep.$(PLATFORM) alicec-mozart.exe \
	alicec-mozart.$(PLATFORM)

seam: alicedep.$(PLATFORM) alicec-seam.$(PLATFORM)

alicec-mozart.exe: $(FUNCTORS)
	$(OZL) $(OZLFLAGS) -x Main.ozf -o $@

%.aml: %.aml.m4
	$(M4) -DPICKLE_VERSION=$(PICKLE_VERSION) $< > $@

%.ozf: $(SRCDIR)/compiler/backend-mozart/%.oz
	$(OZC) $(OZCFLAGS) -c $< -o $@

CodeStore.ozf: $(SRCDIR)/compiler/backend-mozart/CodeEmitter.oz
Prebound.ozf: $(SRCDIR)/vm-mozart/Prebound.oz
	$(OZC) $(OZCFLAGS) -c $< -o $@

alicedep.$(PLATFORM):
	-rm -f $@
	echo 'use "make-alicedep.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

alicec-mozart.$(PLATFORM): $(SRCDIR)/compiler/backend-mozart/PickleOutStream.aml
	-rm -f $@
	echo 'use "make-alicec-mozart.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

alice-mozart.$(PLATFORM): $(SRCDIR)/compiler/backend-mozart/PickleOutStream.aml
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
	-rm -rf CM .cm
	-rm -f alicec-mozart.exe
	-rm -f alicedep.*
	-rm -f alicec-mozart.* alice-mozart.*
	-rm -f alicec-dotnet.*
	-rm -f alicec-seam.*
	-rm -f $(SRCDIR)/compiler/backend-mozart/PickleOutStream.aml

distclean: veryclean
