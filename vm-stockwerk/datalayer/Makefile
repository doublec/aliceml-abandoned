TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = Alice.cc
OBJS = $(SRCS:%.cc=%.o)

all: libdatalayer.a

libdatalayer.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@ 

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libdatalayer.a

distclean: veryclean
	rm -f Makefile.deps

Makefile.deps: Makefile $(SRCS)
	$(MAKEDEPEND) $(SRCS) > Makefile.deps

include Makefile.deps
