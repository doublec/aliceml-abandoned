TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = \
	GlobalPrimitives.cc Primitive.cc Internal.cc Unqualified.cc \
	Array.cc Char.cc Future.cc General.cc GlobalStamp.cc Hole.cc Int.cc \
	List.cc Math.cc Option.cc Real.cc String.cc Thread.cc Unsafe.cc \
	Vector.cc Word.cc
OBJS = $(SRCS:%.cc=%.o)

#--** CommandLine, OS, TextIO do not belong here

all: libbuiltins.a

libbuiltins.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libbuiltins.a

distclean: veryclean
	rm -f Makefile.deps

Makefile.deps: Makefile $(SRCS)
	$(MAKEDEPEND) $(SRCS) > Makefile.deps

include Makefile.deps
