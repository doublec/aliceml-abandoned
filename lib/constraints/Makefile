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

PREFIX = /opt/stockhausen-devel

SRCTOP = ../..
BOOTSTRAPDIR = ../../bootstrap

# No configuration needed from here

SRCDIR = $(SRCTOP)/lib/constraints
VPATH = $(SRCDIR)

OZC = ozc
OZCOPTS = -z9
STOC = alicec
STOCOPTS = --no-dump-elaboration-sig
STODEP = sml @SMLload=$(BOOTSTRAPDIR)/stodep

OZSOURCES = FD.oz FS.oz Space.oz Explorer.oz
ALICESOURCES = SEARCH-sig.aml Search.aml

COMPONENTS = \
	$(OZSOURCES:%.oz=%.ozf) \
	$(ALICESOURCES:%.aml=%.ozf)

INSTALLDIRS = $(PREFIX)/lib/constraints
INSTALLFILES = $(COMPONENTS:%=$(PREFIX)/lib/constraints/%)

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: $(COMPONENTS)

depend:
	$(STODEP) $(ALICESOURCES) > Makefile.depend

Untyped%.ozf: %.oz
	$(OZC) $(OZCOPTS) -c $< -o $@

%.ozf: Untyped%.ozf %.asig
	$(STOC) $(STOCOPTS) --replacesign $^ $@

%.ozf: %.aml
	$(STOC) $(STOCOPTS) -c $< -o $@

-include Makefile.depend

##
## Installation
##

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

$(PREFIX)/lib/constraints/%.ozf: %.ozf
	install -c -m 444 $< $@

##
## Cleaning Up
##

clean:
	-rm -f $(OZSOURCES:%.oz=Untyped%.ozf) $(COMPONENTS)

veryclean: clean

distclean: veryclean
	-rm -f Makefile.depend
