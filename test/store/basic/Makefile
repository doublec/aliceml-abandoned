TOPDIR = ../../..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

ifdef WINDOWS
EXTRA_LIBS = -lwsock32 -lmsvcrt
else
EXTRA_LIBS = -lm
endif

SRCS = test.cc \
       wk.cc \
       blktbl.cc \
       future.cc \
       big.cc
#       perf.cc \

OBJS = $(SRCS:%.cc=%.o)
EXES = $(OBJS:%.o=%.exe)

%.exe: %.o
	$(LD) $(CXXFLAGS) -o $@ $< \
        -L$(TOPDIR)/adt -L$(TOPDIR)/store -L$(TOPDIR)/generic -lstore -ladt -lgeneric $(EXTRA_LIBS)


all: $(OBJS) $(EXES)

store:
	cd ../../../store && $(MAKE) distclean && $(MAKE)

adt:
	cd ../../../adt && $(MAKE) distclean && $(MAKE)

clean:
	rm -f $(OBJS) $(EXES)

distclean: clean
	rm -rf *~
