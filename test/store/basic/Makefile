TOPDIR = ../../..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = test.cc \
       perf.cc

OBJS = test.o \
       perf.o

all: $(OBJS)
	cd $(TOPDIR)/adt && make distclean all
	$(LD) $(CXXFLAGS) -o perf perf.o -L$(TOPDIR)/adt -L$(TOPDIR)/store -lstore -ladt

store:
	cd ../../../store && $(MAKE) distclean && $(MAKE)

clean:
	rm -f test perf $(OBJS)

distclean: clean
	rm -rf *~
