TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

OBJS = AbstractInterpreter.o

.PHONY: all header clean distclean

all: libinterpreter.a

libinterpreter.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libinterpreter.a

distclean: veryclean
