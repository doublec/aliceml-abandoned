INCLUDES = \
	-I$(top_srcdir)			\
	-I$(top_srcdir)/gdk_imlib	\
	@GTK_CFLAGS@

bin_PROGRAMS = imlib_config

imlib_config_SOURCES = \
	imlib_config.c	\
	icons.c		\
	testimg.c

imlib_config_LDADD = \
	$(top_builddir)/gdk_imlib/libgdk_imlib.la	\
	@GTK_LIBS@					\
	@GDK_SUPPORT_LIBS@
