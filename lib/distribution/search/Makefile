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

#PREFIX = /opt/alice-devel

SRCTOP = ../../../
BOOTSTRAPDIR = $(SRCTOP)/bootstrap

# No configuration needed from here

ALICEC = alicec
ALICEDEP = alicedep
ALICEC_OPTS = --rtt-level=no

SOURCES = SERVER-sig.aml Server.aml \
          WORKER_DICT-sig.aml WorkerDict.aml \
          Trace.aml TRACE_SPACE-sig.aml TYPES_CONV-sig.aml MkTraceSpace.aml \
          LOGGER-sig.aml \
          SCRIPT-sig.aml \
          MANAGER-sig.aml \
	  PLAIN_WORKER-sig.aml BEST_WORKER-sig.aml \
          MkManager.aml \
          MkPlainWorker.aml \
          MkBestWorker.aml \
          WORKER_PARCEL-sig.aml \
          PROCESS-sig.aml Process.aml \
          PAR_SEARCH-sig.aml ParSearch.aml \
          REMOTE_WORKER-sig.aml RemoteWorker.aml

COMPONENTS = $(SOURCES:%.aml=%.ozf)

INSTALLDIRS = $(PREFIX)/lib/distribution/search
INSTALLFILES = $(COMPONENTS:%=$(PREFIX)/lib/distribution/search/%)

.PHONY: all depend clean veryclean distclean

all: $(COMPONENTS)

depend:
	$(ALICEDEP) $(ALICESOURCES) > Makefile.depend

$(COMPONENTS): %.ozf: %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

##
## Installing
##

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

$(PREFIX)/lib/distribution/search/%: %
	install -c -m 444 $< $@

##
## Cleaning Up
##

clean:
	rm -f $(COMPONENTS)

veryclean: clean

distclean: veryclean
	rm -f Makefile.depend

##
## Dependencies
##

-include Makefile.depend
