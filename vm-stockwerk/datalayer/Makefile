TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

OBJS = alicedata.o

all: libdatalayer.a

libdatalayer.a: $(OBJS)
	ar rcs $@ $(OBJS)

alicedata.o: alicedata.cc alicedata.hh

clean:
	rm -f $(OBJS) libdatalayer.a

distclean: clean
