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
OZL      = ozl
OZLFLAGS = -z9
SMLC     = stoc
SMLFLAGS =

OZSRCS   = CreateObjects.oz \
           LayoutObjects.oz \
           DrawObjects.oz   \
           TreeNodes.oz     \
           Helper.oz        \
           InspectorMain.oz

OZOBJS   = CreateObjects.ozf \
           LayoutObjects.ozf \
           DrawObjects.ozf   \
           TreeNodes.ozf     \
           Helper.ozf        \
           InspectorMain.ozf

SOURCES  = Inspector.ozf.sig

PREFIX=/opt/stockhausen-devel0

all: $(OZOBJS) $(SMLOBJS)
	$(OZL) $(OZLFLAGS) InspectorMain.ozf -o Inspector.ozf

%.ozf : %.oz
	$(OZC) $(OZFLAGS) -c $< -o $@

clean:
	rm -f $(OZOBJS) $(SMLOBJS) *.ozm *.ozp

install: all
	(cd $(PREFIX)/lib && rm -f Inspector.ozf Inspector.ozf.sig) || exit 1
	cp Inspector.ozf $(PREFIX)/lib
	cp $(SOURCES) $(PREFIX)/lib
