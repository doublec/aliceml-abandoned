TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

OBJS = alicedata.o

all: libdatalayer.a

libdatalayer.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@ 

alicedata.o: alicedata.cc alicedata.hh

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libdatalayer.a

distclean: veryclean
