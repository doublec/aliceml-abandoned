TOPDIR = ../..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

OBJS = Environment.o BootstrapInterpreter.o

.PHONY: all header clean distclean

all: libbootstrapinterpreter.a

libbootstrapinterpreter.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libbootstrapinterpreter.a

distclean: veryclean
