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

SRCS = Base.cc Main.cc
OBJS = $(SRCS:%.cc=%.o)

LIBS = $(SUBDIRS:%=-L%) -lalice -lgeneric -ladt -lstore $(EXTRA_LIBS)

.PHONY: all-subdirs depend-local

all: all-subdirs stow$(EXE)

stow$(EXE): $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

all-subdirs:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) all) || exit 1; done

clean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) clean) || exit 1; done
	rm -f $(OBJS)

veryclean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) veryclean) || exit 1; done
	rm -f $(OBJS) stow stow.exe

distclean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) distclean) || exit 1; done
	rm -f $(OBJS) stow stow.exe Makefile.deps

Makefile.deps: Makefile $(SRCS)
	cd store && $(MAKE) StoreConfig.hh || exit 1
	$(MAKEDEPEND) $(SRCS) > Makefile.deps

include Makefile.deps
