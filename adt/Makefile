## -*- Makefile -*-
##
## Author:
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Leif Kornstaedt, 2000-2003
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = ChunkMap.cc Outline.cc
OBJS = $(SRCS:%.cc=%.o)

all: $(OBJS)

clean:
	rm -f $(OBJS)

veryclean: clean

distclean: veryclean
	rm -f Makefile.depend

Makefile.depend: Makefile $(SRCS)
	$(MAKEDEPEND) $(SRCS) > $@

include Makefile.depend
