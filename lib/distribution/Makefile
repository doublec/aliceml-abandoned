###
### Authors:
###   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
### 
### Copyright:
###   Thorsten Brunklaus, 2001
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

OZC = ozc
OZCOPTS = -z9

STOC = alicec
STODEP =  sml @SMLload=$(BOOTSTRAPDIR)/alicedep
STOCOPTS = --rtt-level=no

OZSOURCES = UnsafePackage.oz UnsafeUnix.oz UnsafeRemoteCore.oz
COMPONENTS = $(OZSOURCES:%.oz=%.ozf)
UNTYPEDCOMPONENTS = $(OZSOURCES:%.oz=Untyped%.ozf)

ALICESOURCES = PACKER-sig.aml Packer.aml \
               REMOTE_CORE-sig.aml RemoteCore.aml \
               REMOTE-sig.aml Remote.aml

ALICECOMPONENTS = $(ALICESOURCES:%.aml=%.ozf)

INSTALLDIRS = $(PREFIX)/lib/distribution
INSTALLFILES = $(COMPONENTS:%=$(PREFIX)/lib/distribution/%) \
               $(ALICECOMPONENTS:%=$(PREFIX)/lib/distribution/%)

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: $(COMPONENTS) $(ALICECOMPONENTS)

depend:
	$(STODEP) $(ALICESOURCES) > Makefile.depend

Untyped%.ozf: %.oz
	$(OZC) $(OZCOPTS) -c $< -o $@

$(COMPONENTS):%.ozf: Untyped%.ozf
	$(STOC) $(STOCOPTS) --replacesign $< $*.asig $@

$(ALICECOMPONENTS): %.ozf: %.aml
	$(STOC) $(STOCOPTS) -c $< -o $@

##
## Installing
##

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

$(PREFIX)/lib/distribution/%: %
	install -c -m 444 $< $@


##
## Cleaning Up
##

clean:
	-rm -f $(UNTYPEDCOMPONENTS) $(ALICECOMPONENTS) $(COMPONENTS)

veryclean: clean

distclean: veryclean
	-rm -f Makefile.depend

##
## Dependencies
##

# Oz Components

UntypedUnsafePackage.ozf: \
	UnsafePackage.oz

UnsafePackage.ozf: \
	UnsafePackage.asig

UntypedUnsafeUnix.ozf: \
	UnsafeUnix.oz

UnsafeUnix.ozf: \
	UnsafeUnix.asig

UntypedUnsafeRemoteCore.ozf: \
	UnsafeRemoteCore.oz

UnsafeRemoteCore.ozf: \
	UnsafeRemoteCore.asig

# Alice Components

-include Makefile.depend
