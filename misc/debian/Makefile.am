## Process this file with automake to produce Makefile.in

SUBDIRS = debian
EXTRA_DATADIR_SUBDIRS = alice/compiler alice/doc \
	alice/lib alice/tools
EXTRA_DATADIR_DIST = \
	alice/alice.import alice/alicec.import \
	applications/alice.desktop pixmaps/alice.png
EXTRA_BINDIR_DIST = \
	alice alicec alicedep aliceglade \
	alicelink alicelex aliceremote aliceyacc


dist-hook:
	(cd bin && $(AMTAR) --exclude '*.dll' -c \
	 $(EXTRA_BINDIR_DIST)) | \
	(cd $(distdir) && mkdir bin && cd bin && $(AMTAR) x)
	(cd share && $(AMTAR) --exclude '*.dll' -c \
	 $(EXTRA_DATADIR_DIST) $(EXTRA_DATADIR_SUBDIRS)) | \
	(cd $(distdir) && mkdir share && cd share && $(AMTAR) x)

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(bindir)
	$(mkinstalldirs) $(DESTDIR)$(datadir)
	(cd $(srcdir)/bin && $(AMTAR) -c $(EXTRA_BINDIR_DIST)) | \
	(cd $(DESTDIR)/$(bindir) && $(AMTAR) x)
	(cd $(srcdir)/share && $(AMTAR) -c $(EXTRA_DATADIR_DIST)) | \
	(cd $(DESTDIR)/$(datadir) && $(AMTAR) x)
	(cd $(srcdir)/share && $(AMTAR) -c $(EXTRA_DATADIR_SUBDIRS)) | \
	(cd $(DESTDIR)/$(datadir) && $(AMTAR) x)
