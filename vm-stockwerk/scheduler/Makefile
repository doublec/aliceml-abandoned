TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

OBJS = Thread.o ThreadPool.o TaskStack.o Scheduler.o

.PHONY: all header clean distclean

all: libscheduler.a

libscheduler.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libscheduler.a

distclean: veryclean
