TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SUBDIRS = bootstrap

all:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) all) || exit 1; done

clean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) clean) || exit 1; done

distclean: clean
