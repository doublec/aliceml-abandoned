## Process this file with automake to produce Makefile.in

EXTRA_SUBDIRS = compiler doc lib tools

EXTRA_DIST = alice.import alicec.import

dist-hook:
	cp -p -r $(EXTRA_SUBDIRS) $(distdir)

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(datadir)/alice
	(cd $(srcdir) && $(AMTAR) -c $(EXTRA_DIST) $(EXTRA_SUBDIRS)) | \
	(cd $(DESTDIR)$(datadir)/alice && $(AMTAR) x)
