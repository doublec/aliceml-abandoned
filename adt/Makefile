TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = Outline.cc HashTable.cc
OBJS = $(SRCS:%.cc=%.o)

all: libadt.a

libadt.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libadt.a

distclean: veryclean

Makefile.deps: Makefile $(SRCS)
	$(MAKEDEPEND) $(SRCS) > Makefile.deps

include Makefile.deps
