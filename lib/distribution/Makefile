###
### Authors:
###   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
### 
### Copyright:
###   Thorsten Brunklaus, 2001
###   Leif Kornstaedt, 2001
### 
### Last change:
###   $Date$ by $Author$
###   $Revision$
### 

# Configurable directories

#PREFIX=/opt/stockhausen-devel

SRCTOP = ../..
BOOTSTRAPDIR = $(SRCTOP)/bootstrap

# No configuration needed from here

SRCDIR = $(SRCTOP)/lib/constraints
VPATH = $(SRCDIR)

OZC = ozc
OZCOPTS = -z9
STOC = alicec
STOCOPTS = 
STODEP = sml @SMLload=$(BOOTSTRAPDIR)/alicedep
STOL = alicelink
STOLOPTS = -v

ALICESOURCES = REMOTE-sig.aml Remote.aml RemoteServer.aml

NATIVES = Remote

OZSOURCES = $(NATIVES:%=Unsafe%.oz)

COMPONENTS = \
	$(NATIVES:%=Linked%.ozf) \
	$(ALICESOURCES:%.aml=%.ozf)

INSTALLDIRS = $(PREFIX)/lib/distribution
INSTALLFILES = $(ALICESOURCES:%.aml=$(PREFIX)/lib/distribution/%.ozf)

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: $(COMPONENTS) $(ALICECOMPONENTS)

depend:
	$(STODEP) $(ALICESOURCES) > Makefile.depend

%.ozf: %.oz
	$(OZC) $(OZCOPTS) -c $< -o $@

Linked%.ozf: %.ozf
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

$(NATIVES:%=$(PREFIX)/lib/distribution/%.ozf): \
	$(PREFIX)/lib/distribution/%.ozf: Linked%.ozf
	install -c -m 444 $< $@

$(PREFIX)/lib/distribution/%.ozf: %.ozf
	install -c -m 444 $< $@

##
## Cleaning Up
##

clean:
	-rm -f $(NATIVES:%=Unsafe%.ozf) $(COMPONENTS) *.ozm

veryclean: clean

distclean: veryclean
	-rm -f Makefile.depend
