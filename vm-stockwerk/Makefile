##
## Author:
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Leif Kornstaedt, 2000-2002
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = .

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SUBDIRS = store adt generic alice
SUBDIRSR = alice generic adt store

SRCS = Base.cc StockwerkMain.cc
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

%.def: $(OBJS) $(LIBS)
	$(DLLTOOL) --output-def $@ --dllname $*.dll $^

%.dll: %.def $(OBJS) $(LIBS)
	$(DLLWRAP) $(LDFLAGS) -o $@ --def $< $(OBJS) $(LDLIBS)

all-subdirs:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) all) || exit 1; done

clean:
	for i in $(SUBDIRSR); do (cd $$i && $(MAKE) clean) || exit 1; done
	rm -f $(OBJS) Main.o stow.def

veryclean:
	for i in $(SUBDIRSR); do (cd $$i && $(MAKE) veryclean) || exit 1; done
	rm -f $(OBJS) Main.o stow.exe stow.dll

distclean:
	for i in $(SUBDIRSR); do (cd $$i && $(MAKE) distclean) || exit 1; done
	rm -f $(OBJS) Main.o stow.exe stow.dll Makefile.depend

Makefile.depend: Makefile $(SRCS)
	cd store && $(MAKE) StoreConfig.hh || exit 1
	$(MAKEDEPEND) $(SRCS) > Makefile.depend

include Makefile.depend
