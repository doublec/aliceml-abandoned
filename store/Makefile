TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = store.cc
OBJS = $(SRCS:%.cc=%.o)

.PHONY: all clean distclean

HEADERWIDTH      = 31
TAGWIDTH         = 16
SIZEWIDTH        = 12
GENERATIONSWIDTH = 2

all: libstore.a #test

libstore.a: headerdef.hh $(OBJS)
	ar rc libstore.a $(OBJS) || exit 1
	ranlib libstore.a

test: test.o
	$(CXX) $(CXXFLAGS) -o $@ $< -L../store -lstore

headerdef.hh: headerdef
	./headerdef $(HEADERWIDTH) $(TAGWIDTH) $(SIZEWIDTH) $(GENERATIONSWIDTH)

headerdef: headerdef.o
	$(CXX) $(CXXFLAGS) -o $@ $<

$(OBJS): headerdef.hh

clean:
	rm -f $(OBJS)

veryclean:
	rm -f test headerdef.hh libstore.a

distclean: veryclean
