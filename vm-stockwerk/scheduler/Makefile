TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

OBJS = Outline.o Scheduler.o

.PHONY: all header clean distclean

all: libscheduler.a

libscheduler.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libscheduler.a

distclean: veryclean
