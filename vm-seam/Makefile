TOPDIR = .

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SUBDIRS = store scheduler datalayer builtins interpreter

all:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) all) || exit 1; done

clean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) clean) || exit 1; done

veryclean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) veryclean) || exit 1; done

distclean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) distclean) || exit 1; done
