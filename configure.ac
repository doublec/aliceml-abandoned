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
AC_SUBST(INCLUDES)
AC_SUBST(LIBS)

dnl
dnl  zlib
dnl
AC_CACHE_CHECK([whether to build with zlib], [seam_cv_with_zlib],
  [AC_ARG_WITH([zlib],
    AC_HELP_STRING([--with-zlib=DIR],
		   [use the zlib file compression library
		    (default: yes if found)]),
    [seam_cv_with_zlib="$with_zlib"],
    [ac_save_LIBS="$LIBS"
     LIBS="-lz $LIBS"
     AC_LINK_IFELSE(AC_LANG_PROGRAM([[#include <zlib.h>]],
				    [[gzFile f = gzopen("a", "rb");]]),
		    seam_cv_with_zlib=yes,
		    LIBS="$ac_save_LIBS"
		    seam_cv_with_zlib=no)])])

case "$seam_cv_with_zlib" in
    yes)
	AC_DEFINE(HAVE_ZLIB, 1)
	;;
    no)
	AC_DEFINE(HAVE_ZLIB, 0)
	;;
    *)
	AC_DEFINE(HAVE_ZLIB, 1)
	INCLUDES="$INCLUDES -I$seam_cv_with_zlib/include"
	LIBS="$LIBS -L$seam_cv_with_zlib/lib -lz"
	;;
esac

dnl
dnl  GNU Lightning
dnl
AC_CACHE_CHECK([whether to build with GNU Lightning], [seam_cv_with_lightning],
  [AC_ARG_WITH([lightning],
    AC_HELP_STRING([--with-lightning=DIR],
		   [use GNU Lightning to enable run-time native
		    code generation (default: yes if found)]),
    [seam_cv_with_lightning="$with_lightning"],
    [AC_COMPILE_IFELSE(AC_LANG_SOURCE([[#include <lightning.h>]]),
		       seam_cv_with_lightning=yes,
		       seam_cv_with_lightning=no)])])

case "$seam_cv_with_lightning" in
    yes)
	AC_DEFINE(HAVE_LIGHTNING, 1)
	;;
    no)
	AC_DEFINE(HAVE_LIGHTNING, 0)
	;;
    *)
	AC_DEFINE(HAVE_LIGHTNING, 1)
	INCLUDES="$INCLUDES -I$seam_cv_with_lightning/include"
	;;
esac

dnl
dnl  Profiler
dnl
AC_CACHE_CHECK([whether to build with user-level profiling support],
  [seam_cv_enable_profiler],
  [AC_ARG_ENABLE([profiler],
		 AC_HELP_STRING([--enable-profiler],
				[build with user-level profiling support
				 (default: no)]),
		 [seam_cv_enable_profiler="$enable_profiler"],
		 [seam_cv_enable_profiler=no])])

case "$seam_cv_enable_profiler" in
    yes)
	AC_DEFINE(PROFILE, 1)
	;;
    no)
	AC_DEFINE(PROFILE, 0)
	;;
esac

dnl
dnl  These are hacks!
dnl
case $(uname) in
    CYGWIN*)
	LIBS="$LIBS -lwsock32"
	;;
    *)
	LDFLAGS="$LDFLAGS -Wl,--export-dynamic -Ur"
	LIBS="$LIBS -lm -ldl"
	;;
esac

dnl
dnl  Output Files
dnl
AC_CONFIG_FILES([Makefile Makefile.vars seamtool])
AC_OUTPUT
