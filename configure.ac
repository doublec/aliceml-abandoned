dnl
dnl Author:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl
dnl Copyright:
dnl   Leif Kornstaedt, 2003
dnl

dnl     Autoconf requirements
AC_REVISION([$Id$])
AC_PREREQ(2.53)

dnl     information on the package
AC_INIT(Seam, 1.0, stockhausen@ps.uni-sb.de)
AC_CONFIG_SRCDIR(Seam.hh)
AC_CONFIG_SUBDIRS([store adt generic])

dnl     checks for programs
AC_PROG_MAKE_SET
AC_PROG_CXX

dnl     checks for libraries
dnl     checks for header files
dnl     checks for types
dnl     checks for structures
dnl     checks for compiler characteristics
dnl     checks for library functions
dnl     checks for system services

case $(uname) in
    CYGWIN*)
	SEAM_MACHINE_FLAGS=-mno-cygwin
	SEAM_LDFLAGS=
	SEAM_LIBS="-lwsock32 -lmsvcrt"
	SUPPORTDIR=$(pwd)/../alice/support/install/mingw32
	;;
    *)
	SEAM_MACHINE_FLAGS=
	SEAM_LDFLAGS="-Wl,--export-dynamic -Ur"
	SEAM_LIBS="-lm -ldl"
	SUPPORTDIR=$(pwd)/../alice/support/install/linux
	;;
esac
AC_SUBST(SEAM_MACHINE_FLAGS)
AC_SUBST(SEAM_LDFLAGS)
AC_SUBST(SEAM_LIBS)

AC_CONFIG_FILES([Makefile Makefile.vars])
AC_OUTPUT
