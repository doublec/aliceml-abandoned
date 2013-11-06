## Process this file with automake to produce Makefile.in

EXTRA_DIST = changelog control copyright \
	rules alice-runtime.info alice-runtime.spec \
	menu

dist-hook:
	$(AMTAR) -c $(EXTRA_DIST) | \
	(cd $(distdir) && $(AMTAR) x)
