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
EXT = alc
else
NATIVES = Remote
OZC = ozc
OZCOPTS = -z9
EXT = ozf
endif

ALICESOURCES = \
	HttpServerMain.aml HttpClientMain.aml \
	REMOTE-sig.aml BinRemote.aml HttpRemote.aml MkRemote.aml Remote.aml \
	TestServer.aml TestClient.aml \
	RemoteServer.aml

OZSOURCES = $(NATIVES:%=Unsafe%.oz)

COMPONENTS = \
	$(ALICESOURCES:%.aml=%.$(EXT)) \
	LinkedBinRemote.$(EXT) \
	LinkedHttpRemote.$(EXT)
	# $(NATIVES:%=Linked%.$(EXT)) \

INSTALLDIRS = $(PREFIX)/lib/distribution
INSTALLFILES = \
	$(filter-out $(PREFIX)/lib/distribution/MkRemote.$(EXT), \
	  $(ALICESOURCES:%.aml=$(PREFIX)/lib/distribution/%.$(EXT)))

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: $(COMPONENTS)

depend:
	$(ALICEDEP) $(ALICESOURCES) > Makefile.depend

%.$(EXT): %.oz
	$(OZC) $(OZCOPTS) -c $< -o $@

Linked%Remote.alc: %Remote.$(EXT) $(OZSOURCES:%.oz=%.$(EXT)) MkRemote.$(EXT)
	$(ALICELINK) $(ALICELINK_OPTS) $*Remote \
	--include $*Remote --include MkRemote -o $@

Linked%Remote.ozf: %Remote.$(EXT) $(OZSOURCES:%.oz=%.$(EXT)) MkRemote.$(EXT)
	$(ALICELINK) $(ALICELINK_OPTS) $*Remote \
	--include $*Remote --include UnsafeRemote --include MkRemote -o $@

#Linked%.$(EXT): %.$(EXT) $(OZSOURCES:%.oz=%.$(EXT))
#	$(ALICELINK) $(ALICELINK_OPTS) $* \
#	--include $* --include Unsafe$* -o $@

%.$(EXT): %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

-include Makefile.depend

##
## Installation
##

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p -m 775 $@

#$(NATIVES:%=$(PREFIX)/lib/distribution/%.$(EXT)): \
#	$(PREFIX)/lib/distribution/%.$(EXT): Linked%.$(EXT)
#	install -c -m 444 $< $@

$(PREFIX)/lib/distribution/%Remote.$(EXT): Linked%Remote.$(EXT)
	install -c -m 444 $< $@

$(PREFIX)/lib/distribution/%.$(EXT): %.$(EXT)
	install -c -m 444 $< $@

##
## Cleaning Up
##

clean:
	-rm -f $(NATIVES:%=Unsafe%.$(EXT)) $(COMPONENTS) *.ozm

veryclean: clean

distclean: veryclean
	-rm -f Makefile.depend
