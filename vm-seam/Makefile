## -*- Makefile -*-
##
## Author:
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Leif Kornstaedt, 2000
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
OBJS = \
	$(SRCS:%.cc=%.o) \
	alice/libalice.a generic/libgeneric.a adt/libadt.a store/libstore.a

LIBS = $(EXTRA_LIBS)

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
	$(MAKEDEPEND) $(SRCS) > Makefile.deps

include Makefile.deps
