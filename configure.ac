dnl
dnl Author:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
dnl Copyright:
dnl   Leif Kornstaedt, 2003
dnl   Marco Kuhlmann, 2003
dnl

AC_REVISION([$Id$])
AC_PREREQ(2.53)
AC_INIT(SEAM, 0.0.1, seam@ps.uni-sb.de)
AC_CONFIG_SRCDIR(Seam.hh)
AM_INIT_AUTOMAKE

seamlibdir='${libdir}/seam'
AC_SUBST(seamlibdir)

# checks for programs

: ${CXXFLAGS=""} dnl we do not want autoconf's default

AC_PROG_CXX
AC_LANG(C++)

# checks for libraries

AC_SEAM_CHECK_SOCKET_FLAVOR

AC_ARG_WITH(zlib,
   AC_HELP_STRING([--with-zlib],
     [use the zlib file compression library @<:@default=yes@:>@]))

if test "${with_zlib:-yes}" = "no"; then
   AC_DEFINE(HAVE_ZLIB, 0)
else
   if test "${with_zlib}" != "yes"; then
      CPPFLAGS="${CPPFLAGS}${CPPFLAGS:+ }-I${with_zlib}"
      CPPFLAGS="${CPPFLAGS} -I${with_zlib}/include"
      LDFLAGS="${LDFLAGS}${LDFLAGS:+ }-L${with_zlib}/lib"
   fi
   AC_CHECK_LIB(z, gzopen,
     [LIBS="${LIBS}${LIBS:+ }-lz"
      AC_DEFINE(HAVE_ZLIB, 1)],
     [LIBS="zlib.lib${LIBS:+ }${LIBS}"
      AC_LINK_IFELSE(AC_LANG_PROGRAM([[#include <zlib.h>]],
                                     [[gzFile f = gzopen("foo", "rb");]]),
        [AC_MSG_RESULT(zlib.lib)
         AC_DEFINE(HAVE_ZLIB, 1)],
        [AC_MSG_ERROR(cannot find the zlib file compression library)])])
fi

# checks for header files

AC_SEAM_WITH_LIGHTNING

# checks for types

# checks for structures

# checks for compiler characteristics

AC_SEAM_CHECK_DECLSPEC

AC_SEAM_CHECK_CXXFLAG(-pipe)
AC_SEAM_CHECK_LDFLAG([-Wl,--export-dynamic])

# checks for library functions

AC_CHECK_DECL(VirtualAlloc,
   [AC_DEFINE(HAVE_VIRTUALALLOC, 1)],
   [AC_DEFINE(HAVE_VIRTUALALLOC, 0)],
   [[#include <windows.h>]])

# checks for system services

# groups of checks

AC_LINK_IFELSE(AC_LANG_PROGRAM([[#include <signal.h>]],
  [[signal(SIGALRM, 0);]]),
  [AC_DEFINE(HAVE_SIGNAL, 1)],
  [AC_DEFINE(HAVE_SIGNAL, 0)])

AC_CHECK_TYPE(sig_atomic_t,
  [AC_DEFINE(HAVE_SIG_ATOMIC_T)], , [[#include <signal.h>]])

AC_CHECK_DECL(SetConsoleCtrlHandler,
  [AC_DEFINE(HAVE_CONSOLECTRL, 1)],
  [AC_DEFINE(HAVE_CONSOLECTRL, 0)],
  [[#include <windows.h>]])  

# libtool

AC_LIBLTDL_CONVENIENCE
AC_SUBST(LTDLINCL)
AC_SUBST(LIBLTDL)
AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL
AC_CONFIG_SUBDIRS(libltdl)
AC_DEFINE(HAVE_LIBLTDL, 1)
AC_SEAM_ADD_TO_CXXFLAGS_SEAMTOOL(-DHAVE_LIBLTDL=1)

case "$host" in
    *-*-cygwin*)
        EXTRA_LDFLAGS="${EXTRA_LDFLAGS}${EXTRA_LDFLAGS:+ }-no-undefined"
        ;;
esac

AC_SUBST(EXTRA_LDFLAGS)

# -------------------------------------------------------------------------
# File compression
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# GNU lightning
# -------------------------------------------------------------------------

AC_SEAM_ARG_ENABLE_CHECKED
AC_SEAM_ARG_ENABLE_DEBUGGER
AC_SEAM_ARG_ENABLE_OPTIMIZED
AC_SEAM_ARG_ENABLE_PROFILER
AC_SEAM_ARG_ENABLE_WARNINGS

# -------------------------------------------------------------------------
# Output files
# -------------------------------------------------------------------------

AC_CONFIG_FILES([
   Makefile
   adt/Makefile
   generic/Makefile
   store/Makefile
   seamtool])
AC_OUTPUT
