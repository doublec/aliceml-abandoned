dnl
dnl Author:
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
dnl Copyright:
dnl   Marco Kuhlmann, 2003
dnl

AC_REVISION([$Id$])
AC_PREREQ(2.53)
AC_INIT(Alice/SEAM, 1.0.1, alice@ps.uni-sb.de, alice)
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

AC_ARG_WITH(gmp,
   AC_HELP_STRING([--with-gmp],
     [use the gmp arbitrary precision arithmetic library @<:@default=yes@:>@]))

if test "${with_gmp:-yes}" = "no"; then
   AC_MSG_ERROR([Alice/Seam depends on gmp. Please supply the correct paths.])
else
   if test "${with_gmp}" != "yes"; then
      CPPFLAGS="${CPPFLAGS}${CPPFLAGS:+ }-I${with_gmp}"
      CPPFLAGS="${CPPFLAGS} -I${with_gmp}/include"
      LDFLAGS="${LDFLAGS}${LDFLAGS:+ }-L${with_gmp}/lib"
      gmpincludes=-I${with_gmp}/include
      gmplib=-L${with_gmp}/lib
      AC_SUBST(gmpincludes)
      AC_SUBST(gmplib)
   fi
   AC_CHECK_LIB(gmp, __gmpz_init, ,
     [AC_MSG_ERROR(
       [cannot find the gmp library, see http://swox.com/gmp])
     ])
fi


# checks for header files

AC_PATH_SEAM

# checks for types

case $host_os in
    darwin*)
       DEFS="${DEFS}${DEFS:+ }-D_BSD_SOCKLEN_T_"
       alicedll=""
       ;;
    *cygwin*)
       alicedll='${libdir}/seam/alice.dll'
       ;;
    *)
       alicedll=""
       ;;
esac

AC_SUBST(alicedll)

AC_CHECK_SIZEOF(long)

# checks for structures

# checks for compiler characteristics

AC_SEAM_CHECK_DECLSPEC

# checks for library functions

dnl
dnl  Test for ISO C 99 rounding functions
dnl
AC_CHECK_FUNCS([fegetround fesetround])

dnl
dnl  Test for math library
dnl
AC_CHECK_FUNC(fabs,
  [],
  AC_CHECK_LIB(m, fabs,
    LIBS=${LIBS}${LIBS:+ }-lm,
    AC_MSG_ERROR([could not find shared math library])))

dnl
dnl  Test for gettimeofday
dnl
AC_CHECK_FUNCS(gettimeofday)

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
AC_SEAM_ARG_WITH_WARNINGS

AC_CONFIG_FILES([
   Makefile
   alice/Makefile
   alice/lib/Makefile
   alice/lib/system/Makefile
   alice/lib/utility/Makefile
   alice/lib/distribution/Makefile
   alice/primitives/Makefile
   debian/Makefile
   bin/alicerun
   bin/alicetool
   bin/alice-config
   Makefile.bootstrap
])

AC_OUTPUT
