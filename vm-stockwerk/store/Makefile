TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = store.cc
OBJS = $(SRCS:%.cc=%.o)

.PHONY: all header clean distclean

HEADERWIDTH      = 31
TAGWIDTH         = 16
SIZEWIDTH        = 11
GENERATIONSWIDTH = 2

all: libstore.a #test

libstore.a: $(OBJS)
	ar rcs libstore.a $(OBJS)

test: test.o
	$(CXX) $(CXXFLAGS) -o $@ $< -L../store -lstore

header: headerdef
	./headerdef $(HEADERWIDTH) $(TAGWIDTH) $(SIZEWIDTH) $(GENERATIONSWIDTH)

headerdef: headerdef.o
	$(CXX) $(CXXFLAGS) -o $@ $<

clean:
	rm -f $(OBJS)

veryclean:
	rm -f test libstore.a

distclean: veryclean
