dnl
dnl Author:
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
dnl Copyright:
dnl   Marco Kuhlmann, 2003
dnl

AC_REVISION([$Id$])
AC_PREREQ(2.53)
AC_INIT(Alice/SEAM, 0.0.20030716, alice@ps.uni-sb.de, alice)
AC_CONFIG_SRCDIR(AliceMain.cc)
AM_INIT_AUTOMAKE

AC_CANONICAL_HOST

# checks for programs

: ${CXXFLAGS=""} dnl we do not want autoconf's default

AC_PROG_SEAMTOOL([],
   [AC_MSG_ERROR([seamtool not found.])])
AC_PROG_CXX
AC_LANG(C++)
AC_PROG_RANLIB

# checks for libraries

AC_SEAM_CHECK_SOCKET_FLAVOR

# checks for header files

AC_PATH_SEAM
AC_SEAM_WITH_LIGHTNING

# checks for types

# checks for structures

# checks for compiler characteristics

AC_SEAM_CHECK_DECLSPEC

# checks for library functions

dnl
dnl  Test for math library
dnl
AC_CHECK_FUNC(fabs,
  [],
  AC_CHECK_LIB(m, fabs,
    LIBS=${LIBS}${LIBS:+ }-lm,
    AC_MSG_ERROR([could not find shared math library])))

# checks for system services

# groups of checks

dnl
dnl  Test for Unix vs. Windows path separator
dnl
AC_MSG_CHECKING(what path separator to use)
case "$host" in
    *-*-cygwin*)
	CACHESEP=";"
	;;
    *-*-*)
        CACHESEP=":"
	;;
esac
AC_MSG_RESULT(${CACHESEP})
AC_SUBST(CACHESEP)

AC_SEAM_ARG_ENABLE_CHECKED
AC_SEAM_ARG_ENABLE_DEBUGGER
AC_SEAM_ARG_ENABLE_OPTIMIZED
AC_SEAM_ARG_ENABLE_PROFILER
AC_SEAM_ARG_ENABLE_WARNINGS

AC_CONFIG_FILES([
   Makefile
   alice/Makefile
   alice/lib/Makefile
   alice/lib/system/Makefile
   alice/lib/utility/Makefile
   alice/lib/distribution/Makefile
   alice/primitives/Makefile
   bin/alicerun
   bin/alicetool
   Makefile.bootstrap
])

AC_OUTPUT
