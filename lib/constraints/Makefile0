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

OZC      = ozc
OZFLAGS  = -z9
SMLC     = stoc
SMLFLAGS =

OZSRCS   = FD.oz    \
           FS.oz    \
           Space.oz \
           Explorer.oz

OZOBJS   = FD.ozf    \
           FS.ozf    \
           Space.ozf \
           Explorer.ozf

SOURCES  = FD.ozf.sig       \
           FS.ozf.sig       \
           Space.ozf.sig    \
           Explorer.ozf.sig \
           Search.sml       \
           SEARCH.sig

PREFIX=/opt/stockhausen-devel

all: $(OZOBJS) $(SMLOBJS)

%.ozf : %.oz
	$(OZC) $(OZFLAGS) -c $< -o $@

clean:
	rm -f $(OZOBJS) $(SMLOBJS) *.ozm *.ozp

install: all
	(cd $(PREFIX)/lib && rm -rf constraints) || exit 1
	mkdir $(PREFIX)/lib/constraints
	cp $(OZOBJS) $(PREFIX)/lib/constraints
	cp $(SOURCES) $(PREFIX)/lib/constraints
