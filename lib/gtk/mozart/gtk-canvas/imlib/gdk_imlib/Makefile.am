common_ldflags = -avoid-version

libimlib_gif_la_SOURCES = io-gif.c
libimlib_gif_la_LDFLAGS = $(common_ldflags)
libimlib_gif_la_LIBADD  = $(GIFLIBS)

libimlib_jpeg_la_SOURCES = io-jpeg.c
libimlib_jpeg_la_LDFLAGS = $(common_ldflags)
libimlib_jpeg_la_LIBADD  = $(JPEGLIBS)

libimlib_png_la_SOURCES = io-png.c
libimlib_png_la_LDFLAGS = $(common_ldflags)
libimlib_png_la_LIBADD  = $(PNGLIBS)

libimlib_tiff_la_SOURCES = io-tiff.c
libimlib_tiff_la_LDFLAGS = $(common_ldflags)
libimlib_tiff_la_LIBADD  = $(TIFFLIBS)

libimlib_ps_la_SOURCES = io-ps.c
libimlib_ps_la_LDFLAGS = $(common_ldflags)
libimlib_ps_la_LIBADD =

libimlib_bmp_la_SOURCES = io-bmp.c
libimlib_bmp_la_LDFLAGS = $(common_ldflags)
libimlib_bmp_la_LIBADD =

libimlib_xpm_la_SOURCES = io-xpm.c
libimlib_xpm_la_LDFLAGS = $(common_ldflags)
libimlib_xpm_la_LIBADD =

libimlib_ppm_la_SOURCES = io-ppm.c
libimlib_ppm_la_LDFLAGS = $(common_ldflags)
libimlib_ppm_la_LIBADD =

if DYNAMIC_MODULES_WORK

if HAVE_GIF
GIF = libimlib-gif.la
endif

if HAVE_JPEG
JPEG = libimlib-jpeg.la
endif

if HAVE_PNG
PNG = libimlib-png.la
endif

if HAVE_TIFF
TIFF = libimlib-tiff.la
endif

OTHER = \
	libimlib-bmp.la		\
	libimlib-xpm.la		\
	libimlib-ppm.la		\
	libimlib-ps.la

DYNAMIC_LIBS = \
	$(OTHER) $(GIF) $(JPEG) $(TIFF) $(PNG)

else

DYNAMIC_LIBS =

endif

lib_LTLIBRARIES = \
	libgdk_imlib.la		\
	$(DYNAMIC_LIBS)

libgdk_imlib_la_SOURCES =	\
	cache.c      		\
	colors.c     		\
	globals.c    		\
	load.c       		\
	misc.c       		\
	rend.c       		\
	utils.c      		\
	save.c			\
	modules.c


libgdk_imlib_la_LDFLAGS = -version-info 10:8:9
libgdk_imlib_la_LIBADD = $(GMODULE_LIBS)


include_HEADERS = gdk_imlib.h gdk_imlib_private.h gdk_imlib_types.h



DEFS = -DDJPEG_PROG=\"@DJPEG_PROG@\"  -DCJPEG_PROG=\"@CJPEG_PROG@\" \
	-DCONVERT_PATH=\"@CONVERT_PATH@\" \
	-DNETPBM_PATH=\"@NETPBM_PATH@\" -DSYSTEM_IMRC=\"@sysconfdir@/imrc\" \
	-DIMLIB_LIB=\""$(libdir)"\"

INCLUDES = -I. -I$(srcdir) -I.. -I$(srcdir)/.. \
	$(GTK_CFLAGS) $(X_CFLAGS) $(GMODULE_FLAGS) 


