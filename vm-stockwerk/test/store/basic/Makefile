TOPDIR = ../../..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = test.cc \
#       perf.cc \
       wk.cc \
       blktbl.cc \
       future.cc

OBJS = $(SRCS:%.cc=%.o)

all: $(OBJS)
	cd $(TOPDIR)/adt && make distclean all
	$(LD) $(CXXFLAGS) -o test test.o -L$(TOPDIR)/adt -L$(TOPDIR)/store -lstore -ladt
#	$(LD) $(CXXFLAGS) -o perf perf.o -L$(TOPDIR)/adt -L$(TOPDIR)/store -lstore -ladt
	$(LD) $(CXXFLAGS) -o wk wk.o -L$(TOPDIR)/adt -L$(TOPDIR)/store -lstore -ladt
	$(LD) $(CXXFLAGS) -o blktbl blktbl.o -L$(TOPDIR)/adt -L$(TOPDIR)/store -lstore -ladt
	$(LD) $(CXXFLAGS) -o future future.o -L$(TOPDIR)/adt -L$(TOPDIR)/store -lstore -ladt

store:
	cd ../../../store && $(MAKE) distclean && $(MAKE)

clean:
	rm -f test perf wk $(OBJS)

distclean: clean
	rm -rf *~
