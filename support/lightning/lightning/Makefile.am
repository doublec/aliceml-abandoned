DISTCLEANFILES = asm.h core.h funcs.h fp.h

BACKENDS = i386 sparc ppc

if LIGHTNING_MAIN
lightningdir = $(includedir)/lightning

lightning_HEADERS = funcs-common.h core-common.h fp-common.h asm-common.h \
	asm.h core.h funcs.h fp.h
else

noinst_HEADERS = funcs-common.h core-common.h fp-common.h asm-common.h \
	lightning.h
endif

dist-hook:
	cd $(distdir); rm -f $(DISTCLEANFILES) || true
	d=`cd $(distdir); pwd`; \
	cd $(srcdir); \
	for i in $(BACKENDS); do \
	  mkdir $$d/$$i; \
	  for j in $(DISTCLEANFILES); do \
	    cp $$i/$$j $$d/$$i; \
	  done; \
	done
