## Process this file with automake to produce Makefile.in

EXTRA_DIST = changelog control copyright \
	rules alice.install alice-dev.install alice.info alice.spec

dist-hook:
	$(AMTAR) -c $(EXTRA_DIST) | \
	(cd $(distdir) && $(AMTAR) x)
