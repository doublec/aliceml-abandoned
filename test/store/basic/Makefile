TOPDIR = ../../..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = test.cc

OBJS = test.o

all: $(OBJS)
	cd $(TOPDIR)/adt && make distclean all
	$(LD) $(CXXFLAGS) -o test test.o -L$(TOPDIR)/adt -L$(TOPDIR)/store -lstore -ladt

store:
	cd ../../../store && $(MAKE) distclean && $(MAKE)

clean:
	rm -f test $(OBJS)

distclean: clean
	rm -rf *~
