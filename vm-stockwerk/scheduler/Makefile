TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

OBJS = Thread.o ThreadPool.o TaskStack.o Scheduler.o

.PHONY: all header clean distclean

all: libscheduler.o

libscheduler.o: $(OBJS)
	ar rcs $@ $(OBJS)
