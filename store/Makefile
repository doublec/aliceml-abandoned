## -*- Makefile -*-
##
## Authors:
##   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Thorsten Brunklaus, 2000
##   Leif Kornstaedt, 2000-2003
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

LIGHTNING_SOURCES0 = JITStore.cc

ifdef LIGHTNING
LIGHTNING_SOURCES = $(LIGHTNING_SOURCES0)
else
LIGHTNING_SOURCES =
endif

SOURCES = Map.cc WeakMap.cc Heap.cc Store.cc $(LIGHTNING_SOURCES)
OBJS = $(SOURCES:%.cc=%.o)

all: StoreConfig.hh $(OBJS)

StoreConfig.hh: StoreConfig.exe
	./$<

StoreConfig.exe: StoreConfig.o
	$(CXX) $(CXXFLAGS) -o $@ $<

clean:
	rm -f StoreConfig.o StoreConfig StoreConfig.exe
	rm -f $(OBJS) $(LIGHTNING_SOURCES0:%.cc=%.o)

veryclean: clean

distclean: veryclean
	rm -f StoreConfig.hh Makefile.depend

Makefile.depend: Makefile StoreConfig.hh $(SOURCES)
	$(MAKEDEPEND) $(SOURCES) StoreConfig.cc > $@

include Makefile.depend
