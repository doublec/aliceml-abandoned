dnl
dnl Author:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl
dnl Copyright:
dnl   Leif Kornstaedt, 2003
dnl

AC_REVISION([$Id$])
AC_PREREQ(2.53)
AC_INIT(Seam, 1.0, stockhausen@ps.uni-sb.de, seam)
AC_CONFIG_SRCDIR(Seam.hh)
AC_CONFIG_SUBDIRS([store adt generic])

dnl
dnl  Programs
dnl
AC_PROG_MAKE_SET
AC_PROG_CXX
AC_PROG_INSTALL

AC_LANG(C++)

dnl
dnl  Linker Flags
dnl
AC_CACHE_CHECK([whether linker understands --export-dynamic],
  [seam_cv_ldflags_export_dynamic],
  [seam_save_LDFLAGS="$LDFLAGS"
   LDFLAGS="$LDFLAGS -Wl,--export-dynamic"
   AC_LINK_IFELSE(AC_LANG_PROGRAM([],[]),
		  seam_cv_ldflags_export_dynamic=yes,
		  seam_cv_ldflags_export_dynamic=no),
   LDFLAGS="$seam_save_LDFLAGS"])

case "$seam_cv_ldflags_export_dynamic" in
    no)
	;;
    yes)
	LDFLAGS="$LDFLAGS -Wl,--export-dynamic"
	;;
esac

dnl
dnl  Libraries
dnl
AC_CHECK_LIB(wsock32, main)
AC_CHECK_LIB(dl, dlopen)

AC_SUBST(INCLUDES)

dnl
dnl  zlib
dnl
AC_MSG_CHECKING([whether to build with zlib])
AC_ARG_WITH([zlib],
  AC_HELP_STRING([--with-zlib=DIR],
		 [use the zlib file compression library
		  (default: yes, if found)]),
  [seam_with_zlib="$with_zlib"],
  [seam_save_LIBS="$LIBS"
   LIBS="-lz $LIBS"
   AC_LINK_IFELSE(AC_LANG_PROGRAM([[#include <zlib.h>]],
				  [[gzFile f = gzopen("foo", "rb");]]),
		  seam_with_zlib=yes,
		  seam_with_zlib=no)
   LIBS="$seam_save_LIBS"])
AC_MSG_RESULT($seam_with_zlib)

case "$seam_with_zlib" in
    no)
	AC_DEFINE(HAVE_ZLIB, 0)
	;;
    yes)
	AC_DEFINE(HAVE_ZLIB, 1)
	LIBS="-lz $LIBS"
	;;
    *)
	AC_DEFINE(HAVE_ZLIB, 1)
	INCLUDES="$INCLUDES -I$seam_cv_with_zlib/include"
	LIBS="-L$seam_with_zlib/lib -lz $LIBS"
	;;
esac

dnl
dnl  GNU Lightning
dnl
AC_MSG_CHECKING([whether to build with GNU Lightning])
AC_ARG_WITH([lightning],
  AC_HELP_STRING([--with-lightning=DIR],
		 [use GNU Lightning to enable run-time native
		  code generation (default: yes, if found)]),
  [seam_with_lightning="$with_lightning"],
  [AC_LINK_IFELSE(AC_LANG_PROGRAM([[#include <lightning.h>]],
				  [[jit_state lightning;]]),
		  seam_with_lightning=yes,
		  seam_with_lightning=no)])
AC_MSG_RESULT($seam_with_lightning)

case "$seam_with_lightning" in
    no)
	AC_DEFINE(HAVE_LIGHTNING, 0)
	;;
    yes)
	AC_DEFINE(HAVE_LIGHTNING, 1)
	;;
    *)
	AC_DEFINE(HAVE_LIGHTNING, 1)
	INCLUDES="$INCLUDES -I$seam_with_lightning/include"
	;;
esac

dnl
dnl  Profiler
dnl
AC_MSG_CHECKING([whether to build with user-level profiling support])
AC_ARG_ENABLE([profiler],
	      AC_HELP_STRING([--enable-profiler],
			     [build with user-level profiling support
			      (default: no)]),
	      [seam_enable_profiler="$enable_profiler"],
	      [seam_enable_profiler=no])
AC_MSG_RESULT($seam_enable_profiler)

case "$seam_enable_profiler" in
    no)
	AC_DEFINE(PROFILE, 0)
	;;
    yes)
	AC_DEFINE(PROFILE, 1)
	;;
esac

dnl
dnl  Output Files
dnl
AC_CONFIG_FILES([Makefile Makefile.vars seamtool])
AC_OUTPUT
