TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = AbstractInterpreter.cc
OBJS = $(SRCS:%.cc=%.o)

all: libinterpreter.a

libinterpreter.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libinterpreter.a

distclean: veryclean

Makefile.deps: Makefile $(SRCS)
	$(MAKEDEPEND) $(SRCS) > Makefile.deps

include Makefile.deps
