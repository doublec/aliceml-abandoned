###
### Authors:
###   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
### 
### Copyright:
###   Thorsten Brunklaus, 2000
###   Leif Kornstaedt, 2001
### 
### Last change:
###   $Date$ by $Author$
###   $Revision$
### 

# Configurable directories

#PREFIX = /opt/alice-devel

SRCTOP = ../..
BOOTSTRAPDIR = ../../bootstrap

# No configuration needed from here

SRCDIR = $(SRCTOP)/lib/constraints
VPATH = $(SRCDIR)

OZC = ozc
OZCOPTS = -z9
ALICEC = alicec
ALICECOPTS = 
ALICEDEP = alicedep
ALICELINK = alicelink
ALICELINK_OPTS = -v

ALICESOURCES = FD-sig.aml FD.aml FS-sig.aml FS.aml \
               SPACE-sig.aml Space.aml \
               SEARCH-sig.aml Search.aml \
               LINEAR-sig.aml Linear.aml

TOOLSOURCES = EXPLORER-sig.aml Explorer.aml

OZNATIVES = FDCommon.oz
NATIVES = FD FS Space Search
TOOLNATIVES = Explorer

OZSOURCES= $(NATIVES:%=Unsafe%.oz) \
	$(TOOLNATIVES:%=Unsafe%.oz)

COMPONENTS = \
	$(OZNATIVES:%.oz=%.ozf) \
	$(NATIVES:%=Linked%.ozf) \
	$(TOOLNATIVES:%=Linked%.ozf) \
	$(ALICESOURCES:%.aml=%.ozf) \
	$(TOOLSOURCES:%.aml=%.ozf) \

INSTALLDIRS = $(PREFIX)/lib/constraints
INSTALLFILES = \
	$(OZNATIVES:%.oz=$(PREFIX)/lib/constraints/%.ozf) \
	$(ALICESOURCES:%.aml=$(PREFIX)/lib/constraints/%.ozf) \
	$(TOOLSOURCES:%.aml=$(PREFIX)/lib/tools/%.ozf)

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: $(COMPONENTS)

depend:
	$(ALICEDEP) $(ALICESOURCES) $(TOOLSOURCES) > Makefile.depend

%.ozf: %.oz
	$(OZC) $(OZCOPTS) -c $< -o $@

Linked%.ozf: %.ozf $(OZSOURCES:%.oz=%.ozf)
	$(ALICELINK) $(ALICELINK_OPTS) $* \
	--exclude $*-sig --include $* --include Unsafe$* -o $@

%.ozf: %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

-include Makefile.depend

##
## Installation
##

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p -m 775 $@

$(NATIVES:%=$(PREFIX)/lib/constraints/%.ozf): \
	$(PREFIX)/lib/constraints/%.ozf: Linked%.ozf
	install -c -m 444 $< $@

$(TOOLNATIVES:%=$(PREFIX)/lib/tools/%.ozf): \
	$(PREFIX)/lib/tools/%.ozf: Linked%.ozf
	install -c -m 444 $< $@

$(PREFIX)/lib/constraints/%.ozf: %.ozf
	install -c -m 444 $< $@

$(PREFIX)/lib/tools/%.ozf: %.ozf
	install -c -m 444 $< $@

##
## Cleaning Up
##

clean:
	-rm -f $(NATIVES:%=Unsafe%.ozf) \
	$(TOOLNATIVES:%=Unsafe%.ozf) $(COMPONENTS) *.ozm

veryclean: clean

distclean: veryclean
	-rm -f Makefile.depend
