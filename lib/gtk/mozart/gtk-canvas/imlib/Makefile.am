SUBDIRS = @GDK_IMLIB@ Imlib config doc
bin_SCRIPTS = imlib-config

EXTRA_DIST = imlib.spec.in imlib.m4 configure configure.in

dist-hook: imlib.spec
	cp imlib.spec $(distdir)

m4datadir = $(datadir)/aclocal
m4data_DATA = imlib.m4
