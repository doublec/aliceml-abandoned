## -*- Makefile -*-
##
## Author:
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Leif Kornstaedt, 2000
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

PRIMITIVES_SRCS = \
	PrimitiveTable.cc \
	Unqualified.cc Array.cc Char.cc Future.cc General.cc GlobalStamp.cc \
	Hole.cc Int.cc List.cc Math.cc Option.cc Real.cc String.cc Thread.cc \
	Unsafe.cc Vector.cc Word.cc
SRCS = Data.cc $(PRIMITIVES_SRCS:%=primitives/%)
OBJS = $(SRCS:%.cc=%.o)

.PHONY: all header clean distclean

all: libalice.a

libalice.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libalice.a

distclean: veryclean
	rm -f Makefile.deps

Makefile.deps: Makefile $(SRCS)
	$(MAKEDEPEND) $(SRCS) > Makefile.deps

include Makefile.deps
