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

#PREFIX = /opt/stockhausen-devel

SRCTOP = ../..
BOOTSTRAPDIR = ../../bootstrap

# No configuration needed from here

SRCDIR = $(SRCTOP)/lib/constraints
VPATH = $(SRCDIR)

OZC = ozc
OZCOPTS = -z9
STOC = alicec
STOCOPTS = --recursive-compilation # --rtt-level=no
STODEP = sml @SMLload=$(BOOTSTRAPDIR)/alicedep
STOL = alicelink
STOLOPTS = -v

ALICESOURCES = FD-sig.aml FD.aml FS-sig.aml FS.aml \
               SPACE-sig.aml Space.aml \
               SEARCH-sig.aml Search.aml EXPLORER-sig.aml Explorer.aml

NATIVES = FD FS Space Explorer

OZSOURCES=$(NATIVES:%=Unsafe%.oz)

COMPONENTS = \
	$(NATIVES:%=Linked%.ozf) \
	$(ALICESOURCES:%.aml=%.ozf)

INSTALLDIRS = $(PREFIX)/lib/constraints
INSTALLFILES = $(ALICESOURCES:%.aml=$(PREFIX)/lib/constraints/%.ozf)

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: $(COMPONENTS)

depend:
	$(STODEP) $(ALICESOURCES) > Makefile.depend

%.ozf: %.oz
	$(OZC) $(OZCOPTS) -c $< -o $@

Linked%.ozf: %.ozf $(OZSOURCES:%.oz=%.ozf)
	$(STOL) $(STOLOPTS) $< --norelative \
	--rewrite=`pwd`/= \
	--include=`pwd`/$< --include=`pwd`/Unsafe$* -o $@

%.ozf: %.aml
	$(STOC) $(STOCOPTS) -c $< -o $@

-include Makefile.depend

##
## Installation
##

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

$(NATIVES:%=$(PREFIX)/lib/constraints/%.ozf): \
	$(PREFIX)/lib/constraints/%.ozf: Linked%.ozf
	install -c -m 444 $< $@

$(PREFIX)/lib/constraints/%.ozf: %.ozf
	install -c -m 444 $< $@

##
## Cleaning Up
##

clean:
	-rm -f $(NATIVES:%=Unsafe%.ozf) $(COMPONENTS) *.ozm

veryclean: clean

distclean: veryclean
	-rm -f Makefile.depend
