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

#PREFIX=/opt/alice-devel

SRCTOP = ../..
BOOTSTRAPDIR = $(SRCTOP)/bootstrap

# No configuration needed from here

SRCDIR = $(SRCTOP)/lib/constraints
VPATH = $(SRCDIR)

ALICEC = alicec
ALICEC_OPTS = --dump-phases --no-warn-conventions
ALICEDEP = alicedep
ALICELINK = alicelink
ALICELINK_OPTS = -v

ALICESOURCES = \
	STREAM-sig.aml MODEL-sig.aml EVENTLISTENER-sig.aml VIEW-sig.aml \
	DEBUGGER_CONTROLS-sig.aml \
	Stream.aml Model.aml Eventlistener.aml TextView.aml EnvView.aml \
	Client.aml DebuggerControls.aml SourceView.aml
COMPONENTS = \
	$(ALICESOURCES:%.aml=%.alc)

INSTALLDIRS = $(PREFIX)/lib/debugger
INSTALL3DIRS = /c/Programme/SEAM/share/alice/lib/debugger
INSTALLFILES = $(ALICESOURCES:%.aml=$(INSTALLDIRS)/%.alc)
INSTALL3FILES = $(ALICESOURCES:%.aml=$(INSTALL3DIRS)/%.alc)

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: $(COMPONENTS) 

depend: 
	$(ALICEDEP) $(ALICESOURCES) > Makefile.depend

-include Makefile.depend

%.alc: %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

##
## Installation
##

install: $(INSTALLDIRS) $(INSTALLFILES)

build3-install: $(INSTALL3DIRS) $(INSTALL3FILES)

$(INSTALLDIRS):
	mkdir -p -m 775 $@

$(INSTALL3DIRS):
	mkdir -p -m 775 $@

$(INSTALLDIRS)/%.alc: %.alc
	install -c -m 444 $< $@

$(INSTALL3DIRS)/%.alc: %.alc
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
