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

PREFIX = /opt/alice-devel

SRCTOP = ../../../../
BOOTSTRAPDIR = $(SRCTOP)/bootstrap

# No configuration needed from here

ALICEC = alicec
ALICEDEP = alicedep
ALICEC_OPTS = --rtt-level=no

SOURCES = Sample.aml \
          FDAlpha.aml \
          FDQueens.aml \
          FDPhoto.aml \
          Main.aml \

COMPONENTS = $(SOURCES:%.aml=%.ozf)

.PHONY: all depend clean veryclean distclean

all: $(COMPONENTS)

depend:
	$(ALICEDEP) $(ALICESOURCES) > Makefile.depend

$(COMPONENTS): %.ozf: %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

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
