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

SRCS = \
	RootSet.cc TaskStack.cc NativeTaskManager.cc InternalTasks.cc \
	Scheduler.cc Transients.cc Outline.cc
OBJS = $(SRCS:%.cc=%.o)

.PHONY: all header clean distclean

all: libgeneric.a

libgeneric.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libgeneric.a

distclean: veryclean
	rm -f Makefile.deps

Makefile.deps: Makefile $(SRCS)
	$(MAKEDEPEND) $(SRCS) > Makefile.deps

include Makefile.deps
