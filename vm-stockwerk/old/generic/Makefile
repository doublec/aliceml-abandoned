TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = Outline.cc Scheduler.cc
OBJS = $(SRCS:%.cc=%.o)

.PHONY: all header clean distclean

all: libscheduler.a

libscheduler.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libscheduler.a

distclean: veryclean
	rm -f Makefile.deps

Makefile.deps: Makefile $(SRCS)
	$(MAKEDEPEND) $(SRCS) > Makefile.deps

include Makefile.deps
