## -*- Makefile -*-
##
## Authors:
##   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Thorsten Brunklaus, 2000
##   Leif Kornstaedt, 2000
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = WeakDictionary.cc \
       Store.cc
OBJS = $(SRCS:%.cc=%.o)

.PHONY: all clean distclean adt

all: libstore.a

adt:
	cd $(TOPDIR)/adt && $(MAKE) clean all

libstore.a: StoreConfig.hh $(OBJS)
	ar rc libstore.a $(OBJS) || exit 1
	ranlib libstore.a

StoreConfig.hh: StoreConfig$(EXE)
	./$<

StoreConfig$(EXE): StoreConfig.o
	$(CXX) $(CXXFLAGS) -o $@ $<

$(OBJS): StoreConfig.hh

clean:
	rm -f StoreConfig.o StoreConfig StoreConfig.exe
	rm -f $(OBJS) libstore.a

veryclean: clean
	rm -f test StoreConfig.hh libstore.a

distclean: veryclean
	rm -f Makefile.deps

Makefile.deps: Makefile StoreConfig.hh $(SRCS)
	$(MAKEDEPEND) $(SRCS) StoreConfig.cc > Makefile.deps

include Makefile.deps
