## Process this file with automake to produce Makefile.in

SUBDIRS = debian
EXTRA_SUBDIRS = compiler doc lib tools

EXTRA_DIST = alice.import alicec.import

dist-hook:
	$(AMTAR) --exclude '*.dll' -c $(EXTRA_SUBDIRS) | \
	(cd $(distdir) && $(AMTAR) x)
#	cp -p -r $(EXTRA_SUBDIRS) $(distdir)

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(datadir)/alice
	(cd $(srcdir) && $(AMTAR) -c $(EXTRA_DIST) $(EXTRA_SUBDIRS)) | \
	(cd $(DESTDIR)$(datadir)/alice && $(AMTAR) x)
