#
# Author:
#   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
#
# Copyright:
#   Marco Kuhlmann, 2003
#
# Last Change:
#   $Date$
#   $Revision$
#

# This file contains GNU autoconf macros for projects that want
# to provide extensions to the SEAM library using seamtool.  A
# typical configure.ac would call AM_PROG_SEAMTOOL before calling
# AC_PROG_CXX, and later call AM_PATH_SEAM to check the presence
# of the SEAM library and header files.  SEAM extensions will
# normally be installed in seamlibdir (default: ${libdir}/seam).

dnl AM_PROG_SEAMTOOL(ACTION-IF-FOUND, ACTION-IF-NOT-FOUND)
dnl ------------------------------------------------------
dnl Check for the presence of seamtool.  If found, make it the
dnl new compiler.  Set $seamlibdir to the directory where SEAM
dnl extensions should be installed.
AC_DEFUN(AM_PROG_SEAMTOOL,[
   AC_BEFORE([$0], [AC_PROG_CXX])dnl
   AC_BEFORE([$0], [AC_PROG_CPP])dnl
   AC_CHECK_PROG(SEAMTOOL, [seamtool], seamtool, none)
   if test "${SEAMTOOL}" != "none"; then
      CXX="${SEAMTOOL} compile" && export CXX
   fi
   AC_SUBST(SEAMTOOL)
   AC_MSG_CHECKING([where to install SEAM extensions])
   if test "${SEAMTOOL}" != "none"; then
      seamlibdir=$(${SEAMTOOL} config libdir)
   else
      seamlibdir='${libdir}/seam'
   fi
   AC_MSG_RESULT(${seamlibdir})
   AC_SUBST(seamlibdir)
   if test "${SEAMTOOL}" != "none"; then
      ifelse([$1], , :, [$1])
   else
      ifelse([$2], , :, [$2])
   fi
])dnl

dnl AM_PATH_SEAM(ACTION-IF-FOUND, ACTION-IF-NOT-FOUND)
dnl --------------------------------------------------
dnl Check for the presence of the SEAM library and header files.
AC_DEFUN(AM_PATH_SEAM,[
   ac_seam_ok=""
   AC_LANG_PUSH(C++)
   AC_CHECK_LIB(seam, InitSeam, ac_seam_ok="${ac_seam_ok}x")
   AC_CHECK_HEADER(Seam.hh, ac_seam_ok="${ac_seam_ok}x")
   AC_LANG_POP
   if test "${ac_seam_ok}" = "xx"; then
      ifelse([$1], , :, [$1])
   else
      ifelse([$2], , :, [$2])
   fi
])dnl
