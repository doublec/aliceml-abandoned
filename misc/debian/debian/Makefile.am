## Process this file with automake to produce Makefile.in

EXTRA_DIST = changelog control copyright \
	rules alice-runtime.info

dist-hook:
	$(AMTAR) -c $(EXTRA_DIST) | \
	(cd $(distdir) && $(AMTAR) x)
