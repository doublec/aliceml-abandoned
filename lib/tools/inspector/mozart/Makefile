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

# No configuration needed from here

SRCDIR = $(SRCTOP)/lib/inspector
VPATH = $(SRCDIR)

OZC = ozc
OZCOPTS = -z9
OZL = stol
OZLOPTS = -z9
STOC = stoc
STOCOPTS = --no-dump-elaboration-sig

OZSOURCES = \
	CreateObjects.oz LayoutObjects.oz DrawObjects.oz TreeNodes.oz \
	Helper.oz InspectorMain.oz

OZLINPUTS = $(OZSOURCES:%.oz=%.ozf)

INSTALLDIRS = $(PREFIX)/lib/tools
INSTALLFILES = $(PREFIX)/lib/tools/Inspector.ozf

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: Inspector.ozf

depend:
	touch Makefile.depend

UntypedInspector.ozf: $(OZLINPUTS)
	$(OZL) $(OZLOPTS) InspectorMain.ozf -o $@

Inspector.ozf: UntypedInspector.ozf Inspector.asig
	$(STOC) $(STOCOPTS) --replacesign $^ $@

%.ozf: %.oz
	$(OZC) $(OZCOPTS) -c $< -o $@

-include Makefile.depend

##
## Installation
##

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

$(PREFIX)/lib/tools/%.ozf: %.ozf
	install -c -m 444 $< $@

##
## Cleaning Up
##

clean:
	-rm -f $(OZLINPUTS) UntypedInspector.ozf Inspector.ozf

veryclean: clean

distclean: veryclean
	-rm -f Makefile.depend
