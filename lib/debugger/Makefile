###
### Authors:
###   Jens Regenberg <jens@ps.uni-sb.de>
### 
### Copyright:
###   Jens Regenberg, 2002
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

ALICEC = alicec
ALICEC_OPTS = --dump-phases
ALICEDEP = alicedep
ALICELINK = alicelink
ALICELINK_OPTS = -v

ALICESOURCES = \
	STREAM-sig.aml MODEL-sig.aml EVENTLISTENER-sig.aml VIEW-sig.aml \
	DEBUGGER_CONTROLS-sig.aml \
	Stream.aml Model.aml Eventlistener.aml TextView.aml EnvView.aml \
	Client.aml DebuggerControls.aml SourceView.aml
COMPONENTS = \
	$(ALICESOURCES:%.aml=%.stc)

INSTALLDIRS = $(PREFIX)/lib/debugger
INSTALL3DIRS = ../../vm-stockwerk/build3/install/lib/debugger
INSTALLFILES = $(ALICESOURCES:%.aml=$(INSTALLDIRS)/%.stc)
INSTALL3FILES = $(ALICESOURCES:%.aml=$(INSTALL3DIRS)/%.stc)

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: $(COMPONENTS) 

depend: 
	$(ALICEDEP) $(ALICESOURCES) > Makefile.depend

-include Makefile.depend

%.stc: %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

##
## Installation
##

install: $(INSTALLDIRS) $(INSTALLFILES)

build3-install: $(INSTALL3DIRS) $(INSTALL3FILES)

$(INSTALLDIRS):
	mkdir -p $@

$(INSTALL3DIRS):
	mkdir -p $@

$(INSTALLDIRS)/%.stc: %.stc
	install -c -m 444 $< $@

$(INSTALL3DIRS)/%.stc: %.stc
	install -c -m 444 $< $@
##
## Cleaning Up
##

clean:
	-rm -f $(COMPONENTS)

veryclean: clean
	rm -f *~

distclean: veryclean
	-rm -f Makefile.depend
