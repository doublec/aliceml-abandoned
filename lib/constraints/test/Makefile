###
### Authors:
###   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
### 
### Copyright:
###   Thorsten Brunklaus, 2000
###   Leif Kornstaedt, 2001
### 
### Last change:
###   $Date$ by $Author$
###   $Revision$
### 

STOC = alicec
STOCOPTS =

ALICESOURCES = Money.aml Photo.aml
COMPONENTS = $(ALICESOURCES:%.aml=%.ozf)

%.ozf : %.aml
	$(STOC) $(STOCOPTS) -c $< -o $@

.PHONY: all clean veryclean distclean

all: $(COMPONENTS)

clean:
	-rm -f $(COMPONENTS)

veryclean: clean

distclean: veryclean
