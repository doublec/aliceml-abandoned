###
### Authors:
###   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
### 
### Copyright:
###   Thorsten Brunklaus, 2001
###   Leif Kornstaedt, 2001-2003
### 
### Last change:
###   $Date$ by $Author$
###   $Revision$
### 

# Configurable directories

TARGET = mozart

#PREFIX=/opt/alice-devel

SRCTOP = ../..
BOOTSTRAPDIR = $(SRCTOP)/bootstrap

# No configuration needed from here

SRCDIR = $(SRCTOP)/lib/constraints
VPATH = $(SRCDIR)

ALICEC = alicec
ALICEC_OPTS = 
ALICEDEP = alicedep
ALICELINK = alicelink
ALICELINK_OPTS = -v
ifeq ($(TARGET), seam)
NATIVES = 
STC = stc
else
NATIVES = Remote
OZC = ozc
OZCOPTS = -z9
STC = ozf
endif

ALICESOURCES = \
	HttpServerMain.aml HttpClientMain.aml \
	REMOTE-sig.aml BinRemote.aml HttpRemote.aml MkRemote.aml Remote.aml \
	TestServer.aml TestClient.aml \
	RemoteServer.aml

OZSOURCES = $(NATIVES:%=Unsafe%.oz)

COMPONENTS = \
	$(ALICESOURCES:%.aml=%.$(STC)) \
	LinkedBinRemote.$(STC) \
	LinkedHttpRemote.$(STC)
	# $(NATIVES:%=Linked%.$(STC)) \

INSTALLDIRS = $(PREFIX)/lib/distribution
INSTALLFILES = \
	$(filter-out $(PREFIX)/lib/distribution/MkRemote.$(STC), \
	  $(ALICESOURCES:%.aml=$(PREFIX)/lib/distribution/%.$(STC)))

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: $(COMPONENTS)

depend:
	$(ALICEDEP) $(ALICESOURCES) > Makefile.depend

%.$(STC): %.oz
	$(OZC) $(OZCOPTS) -c $< -o $@

Linked%Remote.stc: %Remote.$(STC) $(OZSOURCES:%.oz=%.$(STC)) MkRemote.$(STC)
	$(ALICELINK) $(ALICELINK_OPTS) $*Remote \
	--include $*Remote --include MkRemote -o $@

Linked%Remote.ozf: %Remote.$(STC) $(OZSOURCES:%.oz=%.$(STC)) MkRemote.$(STC)
	$(ALICELINK) $(ALICELINK_OPTS) $*Remote \
	--include $*Remote --include UnsafeRemote --include MkRemote -o $@

#Linked%.$(STC): %.$(STC) $(OZSOURCES:%.oz=%.$(STC))
#	$(ALICELINK) $(ALICELINK_OPTS) $* \
#	--include $* --include Unsafe$* -o $@

%.$(STC): %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

-include Makefile.depend

##
## Installation
##

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p -m 775 $@

#$(NATIVES:%=$(PREFIX)/lib/distribution/%.$(STC)): \
#	$(PREFIX)/lib/distribution/%.$(STC): Linked%.$(STC)
#	install -c -m 444 $< $@

$(PREFIX)/lib/distribution/%Remote.$(STC): Linked%Remote.$(STC)
	install -c -m 444 $< $@

$(PREFIX)/lib/distribution/%.$(STC): %.$(STC)
	install -c -m 444 $< $@

##
## Cleaning Up
##

clean:
	-rm -f $(NATIVES:%=Unsafe%.$(STC)) $(COMPONENTS) *.ozm

veryclean: clean

distclean: veryclean
	-rm -f Makefile.depend
