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

PREFIX = /opt/stockhausen-devel

SRCTOP = ../../../../
BOOTSTRAPDIR = $(SRCTOP)/bootstrap

# No configuration needed from here

STOC = alicec
STODEP =  sml @SMLload=$(BOOTSTRAPDIR)/alicedep
STOCOPTS = --rtt-level=no

SOURCES = Sample.aml

COMPONENTS = $(SOURCES:%.aml=%.ozf)

.PHONY: all depend clean veryclean distclean

all: $(COMPONENTS)

depend:
	$(STODEP) $(ALICESOURCES) > Makefile.depend

$(COMPONENTS): %.ozf: %.aml
	$(STOC) $(STOCOPTS) -c $< -o $@

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
