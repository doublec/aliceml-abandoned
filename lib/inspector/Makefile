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

SRCDIR = $(SRCTOP)/lib/inspector
VPATH = $(SRCDIR)

OZC = ozc
OZCOPTS = -z9
OZLOPTS = -z9
STOC = alicec
STOL = alicelink
STODEP = sml @SMLload=$(BOOTSTRAPDIR)/alicedep
STOCOPTS = --recursive-compilation
OZSOURCES = \
	CreateObjects.oz LayoutObjects.oz DrawObjects.oz TreeNodes.oz \
	Helper.oz UnsafeInspector.oz

OZLINPUTS = $(OZSOURCES:%.oz=%.ozf)

INSTALLDIRS = $(PREFIX)/lib/tools
INSTALLFILES = $(PREFIX)/lib/tools/Inspector.ozf

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: LinkedInspector.ozf

depend:
	$(STODEP) Inspector.aml INSPECTOR-sig.aml > Makefile.depend

Inspector.ozf: UnsafeInspector.asig Inspector.aml
	$(STOC) $(STOCOPTS) -c Inspector.aml -o Inspector.ozf

LinkedInspector.ozf: $(OZLINPUTS) Inspector.ozf
	$(STOL) $(OZLOPTS) Inspector.ozf -o LinkedInspector.ozf

%.ozf: %.oz
	$(OZC) $(OZCOPTS) -c $< -o $@

-include Makefile.depend

##
## Installation
##

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

$(PREFIX)/lib/tools/%.ozf: Linked%.ozf
	install -c -m 444 $< $@

##
## Cleaning Up
##

clean:
	-rm -f $(OZLINPUTS) UnsafeInspector.ozf Inspector.ozf INSPECTOR-sig.ozf LinkedInspector.ozf

veryclean: clean

distclean: veryclean
	-rm -f Makefile.depend
