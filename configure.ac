dnl
dnl Author:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl
dnl Copyright:
dnl   Leif Kornstaedt, 2003
dnl

AC_REVISION([$Id$])
AC_PREREQ(2.53)
AC_INIT(Seam, 1.0, stockhausen@ps.uni-sb.de)
AC_CONFIG_SRCDIR(Seam.hh)
AC_CONFIG_SUBDIRS([store adt generic])

dnl
dnl  Programs
dnl
AC_PROG_MAKE_SET
AC_PROG_CXX

dnl
dnl  zlib
dnl
AC_MSG_CHECKING([whether to build with zlib])
AC_ARG_WITH([zlib],
	    AC_HELP_STRING([--with-zlib=DIR],
			   [use the zlib file compression library
			    (default: no)]),
	    HAVE_ZLIB=1
	    ZLIBINCLUDES="-I$with_zlib/include"
	    ZLIBLIBS="-L$with_zlib/lib -lz"
	    result=yes,
	    HAVE_ZLIB=0
	    ZLIBINCLUDES=""
	    ZLIBLIBS=""
	    result=no)
AC_MSG_RESULT($result)
AC_SUBST(HAVE_ZLIB)
AC_SUBST(ZLIBINCLUDES)
AC_SUBST(ZLIBLIBS)

dnl
dnl  GNU Lightning
dnl
AC_MSG_CHECKING([whether to build with GNU Lightning])
AC_ARG_WITH([lightning],
	    AC_HELP_STRING([--with-lightning=DIR],
			   [use GNU Lightning to enable run-time
			    native code generation (default: no)]),
	    HAVE_LIGHTNING=1
	    LIGHTNINGINCLUDES="-I$with_lightning/include"
	    result=yes,
	    HAVE_LIGHTNING=0
	    LIGHTNINGINCLUDES=""
	    result=no)
AC_MSG_RESULT($result)
AC_SUBST(HAVE_LIGHTNING)
AC_SUBST(LIGHTNINGINCLUDES)

dnl
dnl  Profiler
dnl
AC_MSG_CHECKING([whether to build with user-level profiling support])
AC_ARG_ENABLE([profiler],
	      AC_HELP_STRING([--enable-profiler],
			     [build with user-level profiling support]),
	      PROFILE=1
	      result=yes,
	      PROFILE=0
	      result=no)
AC_SUBST(PROFILE)
AC_MSG_RESULT($result)

dnl
dnl  These are hacks!
dnl
case $(uname) in
    CYGWIN*)
	SEAM_MACHINE_FLAGS=-mno-cygwin
	SEAM_LDFLAGS=
	SEAM_LIBS="-lwsock32 -lmsvcrt"
	SUPPORTDIR=$PWD/../alice/support/install/mingw32
	;;
    *)
	SEAM_MACHINE_FLAGS=
	SEAM_LDFLAGS="-Wl,--export-dynamic -Ur"
	SEAM_LIBS="-lm -ldl"
	SUPPORTDIR=$PWD/../alice/support/install/linux
	;;
esac
AC_SUBST(SEAM_MACHINE_FLAGS)
AC_SUBST(SEAM_LDFLAGS)
AC_SUBST(SEAM_LIBS)
AC_SUBST(SUPPORTDIR)

AC_CONFIG_FILES([Makefile Makefile.vars])
AC_OUTPUT
