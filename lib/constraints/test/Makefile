## -*- Makefile -*-
##
## Author:
##   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
## 
## Copyright:
##   Thorsten Brunklaus, 2000
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

SMLC     = stoc
SMLFLAGS =

SMLSRCS = Money.sml \
          Photo.sml

SMLOBJS = Money.ozf \
          Photo.ozf

all: $(SMLOBJS)

%.ozf : %.sml
	$(SMLC) $(SMLFLAGS) -c $< -o $@

clean:
	rm -f $(SMLOBJS) *.ozp *.ozm
