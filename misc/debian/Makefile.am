## Process this file with automake to produce Makefile.in

SUBDIRS = debian
EXTRA_SUBDIRS = share/alice/compiler share/alice/doc \
	share/alice/lib share/alice/tools

EXTRA_DIST = share/alice/alice.import share/alice/alicec.import \
	bin/alice bin/alicec bin/alicedep bin/aliceglade \
	bin/alicelink bin/alicelex bin/aliceremote bin/aliceyacc \
	debian/alice.desktop

dist-hook:
	$(AMTAR) --exclude '*.dll' -c $(EXTRA_DIST) $(EXTRA_SUBDIRS) | \
	(cd $(distdir) && $(AMTAR) x)

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)
	(cd $(srcdir) && $(AMTAR) -c $(EXTRA_DIST) $(EXTRA_SUBDIRS)) | \
	(cd $(DESTDIR) && $(AMTAR) x)
