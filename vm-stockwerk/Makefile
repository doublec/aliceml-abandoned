##
## Author:
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Leif Kornstaedt, 2000-2003
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = .

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

# GNU make has no reverse function, but we need these in reversed form, too:
SUBDIRS = store adt generic alice java
SUBDIRSR = java alice generic adt store

SRCS = Base.cc AliceMain.cc
OBJS = $(SRCS:%.cc=%.o)
LIBS = $(shell for i in $(SUBDIRS); do echo $$i/lib$$i.a; done)

LDLIBS = $(SUBDIRS:%=-L%) $(SUBDIRSR:%=-l%) \
	-L$(SUPPORTDIR)/lib $(EXTRA_LIBS) -lz

.PHONY: all-subdirs depend-local

all: all-subdirs $(TARGETS)

ifdef WINDOWS
stow.exe: Main.o stow.dll
	$(LD) $(LDFLAGS) -o $@ Main.o stow.dll
else
stow.exe: Main.o $(OBJS) $(LIBS)
	$(LD) $(LDFLAGS) -o $@ Main.o $(OBJS) $(LDLIBS)
endif

java.exe: JavaMain.o Base.o $(LIBS)
	$(LD) $(LDFLAGS) -o $@ $< Base.o $(LDLIBS)

%.dll: $(OBJS) $(LIBS)
	$(LD) $(LDFLAGS) -shared -o $@ $(OBJS) $(LDLIBS)

all-subdirs:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) all) || exit 1; done

clean:
	for i in $(SUBDIRSR); do (cd $$i && $(MAKE) clean) || exit 1; done
	rm -f $(OBJS) Main.o stow.def JavaMain.o

veryclean:
	for i in $(SUBDIRSR); do (cd $$i && $(MAKE) veryclean) || exit 1; done
	rm -f $(OBJS) Main.o stow.def JavaMain.o
	rm -f stow.exe stow.dll java.exe

distclean:
	for i in $(SUBDIRSR); do (cd $$i && $(MAKE) distclean) || exit 1; done
	rm -f $(OBJS) Main.o stow.def JavaMain.o
	rm -f stow.exe stow.dll java.exe
	rm -f Makefile.depend

Makefile.depend: Makefile $(SRCS) JavaMain.cc
	cd store && $(MAKE) StoreConfig.hh || exit 1
	$(MAKEDEPEND) $(SRCS) JavaMain.cc > Makefile.depend

include Makefile.depend
