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

PREFIX = /opt/alice-devel

SRCTOP = ../..
BOOTSTRAPDIR = ../../bootstrap

# No configuration needed from here

SRCDIR = $(SRCTOP)/lib/inspector
VPATH = $(SRCDIR)

OZC = ozc
OZCOPTS = -z9
ALICEC = alicec
ALICEC_OPTS = --implicit-import-file ../fundamental/Fundamental.import
ALICELINK = alicelink
ALICELINK_OPTS = -v
ALICEDEP = alicedep
OZSOURCES = \
	CreateObjects.oz LayoutObjects.oz DrawObjects.oz TreeNodes.oz \
	Helper.oz UnsafeInspector.oz

ALICELINK_INPUTS = $(OZSOURCES:%.oz=%.ozf)

INSTALLDIRS = $(PREFIX)/lib/tools
INSTALLFILES = $(PREFIX)/lib/tools/Inspector.ozf

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: LinkedInspector.ozf

depend:
	$(ALICEDEP) Inspector.aml INSPECTOR-sig.aml > Makefile.depend

%.ozf: %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

Inspector.ozf: UnsafeInspector.asig Inspector.aml

LinkedInspector.ozf: Inspector.ozf $(ALICELINK_INPUTS)
	$(ALICELINK) $(ALICELINK_OPTS) ./Inspector --include ./ -o $@

%.ozf: %.oz
	$(OZC) $(OZCOPTS) -c $< -o $@

-include Makefile.depend

##
## Installation
##

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p -m 775 $@

$(PREFIX)/lib/tools/%.ozf: Linked%.ozf
	install -c -m 444 $< $@

##
## Cleaning Up
##

clean:
	-rm -f $(ALICELINK_INPUTS) UnsafeInspector.ozf Inspector.ozf INSPECTOR-sig.ozf LinkedInspector.ozf *.ozm

veryclean: clean

distclean: veryclean
	-rm -f Makefile.depend
