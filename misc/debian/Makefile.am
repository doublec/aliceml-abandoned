## Process this file with automake to produce Makefile.in

SUBDIRS = bin
EXTRA_SUBDIRS = compiler doc lib tools

man_MANS = man/alice.1 man/alicec.1 \
	man/alicerun.1 man/alicelink.1 \
	man/alicedep.1

EXTRA_DIST = VMMain.ozf alice.import alicec.import $(man_MANS)

dist-hook:
	cp -p -r $(EXTRA_SUBDIRS) $(distdir)

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(datadir)/alice
	(cd $(srcdir) && $(AMTAR) -c $(EXTRA_DIST) $(EXTRA_SUBDIRS)) | \
	(cd $(DESTDIR)$(datadir)/alice && $(AMTAR) x)
