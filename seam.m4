# -*- autoconf -*-

#
# Author:
#   Leif Kornstaedt <kornstae@ps.uni-sb.de>
#   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
#
# Copyright:
#   Leif Kornstaedt, 2003
#   Marco Kuhlmann, 2003
#
# Last Change:
#   $Date$
#   $Revision$
#

# This file contains GNU autoconf macros for the SEAM build
# process and for the build processes of projects that want to
# provide extensions to the SEAM library.

# ---------------------------------------------------------------
# Macros used in the build process of SEAM
# ---------------------------------------------------------------

dnl Macro:
dnl   AC_SEAM_ARG_ENABLE_CHECKED
dnl
dnl Description:
dnl   Introduce a build option to build with debug symbols and
dnl   assertions and take the necessary actions if the option was
dnl   supplied.
dnl
dnl Authors:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_ARG_ENABLE_CHECKED],
  [AC_ARG_ENABLE([checked],
      AC_HELP_STRING([--enable-checked],
        [build with debug symbols and assertions @<:@default=no@:>@]))
   AC_MSG_CHECKING(whether to build with debug symbols and assertions)
   if test "${enable_checked:-no}" = "yes"; then
      AC_MSG_RESULT(yes)
      AC_SEAM_CHECK_CXXFLAG(-fno-inline-functions)
      AC_SEAM_CHECK_CXXFLAG(-fimplement-inlines)
      AC_SEAM_CHECK_CXXFLAG(-ggdb,,
         AC_SEAM_CHECK_CXXFLAG(-g))

      AC_SEAM_CHECK_CXXFLAG_SEAMTOOL(-fno-inline-functions)
      AC_SEAM_CHECK_CXXFLAG_SEAMTOOL(-fimplement-inlines)
      AC_SEAM_CHECK_CXXFLAG_SEAMTOOL(-ggdb,
         AC_SEAM_ADD_TO_CXXFLAGS_SEAMTOOL(-ggdb),
         AC_SEAM_CHECK_CXXFLAG_SEAMTOOL(-g))
      AC_SEAM_ADD_TO_CXXFLAGS_SEAMTOOL(-DINTERFACE \
                                       -DDEBUG_CHECK \
                                       -DSTORE_DEBUG)
      AC_DEFINE(INTERFACE)
      AC_DEFINE(DEBUG_CHECK)
      AC_DEFINE(STORE_DEBUG)
   else
      AC_MSG_RESULT(no)
   fi])dnl

dnl Macro:
dnl   AC_SEAM_ARG_ENABLE_OPTIMIZED
dnl
dnl Description:
dnl   Introduce a build option to build with optimization and
dnl   take the necessary actions if the option was supplied.
dnl
dnl Authors:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_ARG_ENABLE_OPTIMIZED],
  [AC_ARG_ENABLE([optimized],
      AC_HELP_STRING([--enable-optimized],
        [build with optimization @<:@default=yes, unless checked build@:>@]))
   AC_MSG_CHECKING(whether to build with optimization)
   if test "${enable_optimized:-yes}" = "yes" -a \
           "${enable_checked:-no}" = "no"; then
      AC_MSG_RESULT(yes)
      AC_SEAM_CHECK_CXXFLAGS(-O3 -fomit-frame-pointer \
                             -fforce-mem -fforce-addr \
                             -finline-limit=2500 -fno-implement-inlines \
                             -fno-keep-static-consts \
                             -fno-implicit-templates \
                             -fno-implicit-inline-templates)
      AC_SEAM_CHECK_LDFLAG([[-Wl,-S]])
      AC_SEAM_CHECK_LDFLAG_SEAMTOOL([[-Wl,-S]])
   else
      AC_MSG_RESULT(no)
   fi])

dnl Macro:
dnl   AC_SEAM_ARG_ENABLE_PROFILER
dnl
dnl Description:
dnl   Introduce a build option to build with user-level profiling
dnl   support.
dnl
dnl Authors:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_ARG_ENABLE_PROFILER],
  [AC_ARG_ENABLE([profiler],
      AC_HELP_STRING([--enable-profiler],
        [build with user-level profiling support @<:@default=no@:>@]))
   AC_MSG_CHECKING(whether to build with user-level profiling support)
   if test "${enable_profiler:-no}" = "yes"; then
      AC_MSG_RESULT(yes)
      AC_SEAM_ADD_TO_CXXFLAGS_SEAMTOOL([-DPROFILE=1])
      AC_DEFINE(PROFILE, 1)
   else
      AC_MSG_RESULT(no)
      AC_SEAM_ADD_TO_CXXFLAGS_SEAMTOOL([-DPROFILE=0])
      AC_DEFINE(PROFILE, 0)
   fi])

dnl Macro:
dnl   AC_SEAM_CHECK_CXXFLAG (FLAG, [ACTION-IF-TRUE,
dnl                                [ACTION-IF-FALSE]])
dnl
dnl Description:
dnl   Check whether FLAG is supported by the C++ compiler.  Run
dnl   the shell commands ACTION-IF-TRUE if it is, ACTION-IF-FALSE
dnl   otherwise.  If ACTION-IF-TRUE is not given, append FLAG to
dnl   the contents of $CXXFLAGS.
dnl
dnl Authors:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_CHECK_CXXFLAG],
  [AC_MSG_CHECKING(whether ${CXX} accepts [$1])
   ac_seam_save_CXXFLAGS="${CXXFLAGS}"
   CXXFLAGS="${CXXFLAGS}${CXXFLAGS:+ }$1"
   AC_LANG_PUSH(C++)
   AC_COMPILE_IFELSE(AC_LANG_PROGRAM(),
     [AC_MSG_RESULT(yes)
      CXXFLAGS="${ac_seam_save_CXXFLAGS}"
      ifelse([$2], , [CXXFLAGS="${CXXFLAGS}${CXXFLAGS:+ }$1"], [$2])],
     [AC_MSG_RESULT(no)
      CXXFLAGS="${ac_seam_save_CXXFLAGS}"
      ifelse([$3], , :, [$3])])
   AC_LANG_POP])dnl

dnl Macro:
dnl   AC_SEAM_CHECK_CXXFLAGS (FLAGS...)
dnl
dnl Description:
dnl   For each given flag in the whitespace-separated argument
dnl   list, test whether it is supported by the C++ compiler.  If
dnl   it is, append it to the contents of $CXXFLAGS.
dnl
dnl Author:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>

AC_DEFUN([AC_SEAM_CHECK_CXXFLAGS],
  [AC_REQUIRE([AC_SEAM_CHECK_CXXFLAG])
   for ac_seam_cxxflag in $1; do
       AC_SEAM_CHECK_CXXFLAG(${ac_seam_cxxflag})
   done])dnl

dnl Macro:
dnl   AC_SEAM_CHECK_LDFLAG (FLAG, [ACTION-IF-TRUE,
dnl                               [ACTION-IF-FALSE]])
dnl
dnl Description:
dnl   Check whether FLAG is supported by the linker.  Run the
dnl   shell commands ACTION-IF-TRUE if it is, ACTION-IF-FALSE
dnl   otherwise.  If ACTION-IF-TRUE is not given, append FLAG to
dnl   the contents of $LDFLAGS.
dnl
dnl Authors:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_CHECK_LDFLAG],
  [AC_REQUIRE([AC_PROG_CXX])
   AC_MSG_CHECKING(whether the linker accepts [$1])
   ac_seam_save_LDFLAGS="${LDFLAGS}"
   LDFLAGS="${LDFLAGS}${LDFLAGS:+ }$1"
   AC_LINK_IFELSE(AC_LANG_PROGRAM(),
     [AC_MSG_RESULT(yes)
      LDFLAGS="$ac_seam_save_LDFLAGS"
      ifelse([$2], , [LDFLAGS="${LDFLAGS}${LDFLAGS:+ }$1"], [$2])],
     [AC_MSG_RESULT(no)
      LDFLAGS="$ac_seam_save_LDFLAGS"
      ifelse([$3], , :, [$3])])])dnl

dnl Macro:
dnl   AC_SEAM_CHECK_LDFLAGS (FLAGS...)
dnl
dnl Description:
dnl   For each given flag in the whitespace-separated argument
dnl   list, test whether it is supported by the linker.  If it
dnl   is, append it to the contents of $LDFLAGS.
dnl
dnl Author:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_CHECK_LDFLAGS],
  [AC_REQUIRE([AC_SEAM_CHECK_LDFLAG])
   for ac_seam_ldflag in $1; do
       AC_SEAM_CHECK_LDFLAG(${ac_seam_ldflag})
   done])dnl

dnl Macros:
dnl   AC_SEAM_ADD_TO_CXXFLAGS_SEAMTOOL ([FLAG...])
dnl   AC_SEAM_ADD_TO_LDFLAGS_SEAMTOOL ([FLAG...])
dnl
dnl Description:
dnl   While many of the build options determined during the
dnl   configuration process of SEAM itself will not make sense
dnl   when building a new language layer, some of them need to
dnl   be propagated.  This propagation happens through the
dnl   seamtool utility, whose $CXXFLAGS and $LDFLAGS are set by
dnl   the following two macros.
dnl
dnl Author:
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_ADD_TO_CXXFLAGS_SEAMTOOL],
   [SEAMTOOL_CXXFLAGS="${SEAMTOOL_CXXFLAGS}${SEAMTOOL_CXXFLAGS:+ }$1"
    AC_SUBST(SEAMTOOL_CXXFLAGS)])
AC_DEFUN([AC_SEAM_ADD_TO_LDFLAGS_SEAMTOOL],
   [SEAMTOOL_LDFLAGS="${SEAMTOOL_LDFLAGS}${SEAMTOOL_LDFLAGS:+ }$1"
    AC_SUBST(SEAMTOOL_LDFLAGS)])

dnl Macros:
dnl   AC_SEAM_CHECK_CXXFLAG_SEAMTOOL (FLAGS...)
dnl   AC_SEAM_CHECK_LDFLAG_SEAMTOOL (FLAGS...)
dnl
dnl Description:
dnl   Like the macros without the SEAMTOOL suffix above, but
dnl   append options recognised by the compiler to the list of
dnl   options propagated to seamtool.
dnl
dnl Author:
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_CHECK_CXXFLAG_SEAMTOOL],
   [AC_REQUIRE([AC_SEAM_CHECK_CXXFLAG])
    AC_SEAM_CHECK_CXXFLAG($1, AC_SEAM_ADD_TO_CXXFLAGS_SEAMTOOL([$1]))])
AC_DEFUN([AC_SEAM_CHECK_LDFLAG_SEAMTOOL],
   [AC_REQUIRE([AC_SEAM_CHECK_CXXFLAG])
    AC_SEAM_CHECK_LDFLAG($1, AC_SEAM_ADD_TO_LDFLAGS_SEAMTOOL([$1]))])

# ---------------------------------------------------------------
# Macros used in the build process of SEAM extensions
# ---------------------------------------------------------------

dnl Macro:
dnl   AC_PROG_SEAMTOOL (ACTION-IF-FOUND, ACTION-IF-NOT-FOUND)
dnl
dnl Description:
dnl   Check for the presence of seamtool.  If found, make it the
dnl   new compiler.  Set $seamlibdir to the directory where SEAM
dnl   extensions should be installed.
dnl
dnl Author:
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN(AC_PROG_SEAMTOOL,
  [AC_BEFORE([$0], [AC_PROG_CXX])dnl
   AC_BEFORE([$0], [AC_PROG_CPP])dnl
   AC_CHECK_PROG(SEAMTOOL, seamtool, seamtool, none)
   if test "${SEAMTOOL}" != "none"; then
      CXX="${SEAMTOOL} compile" && export CXX
   fi
   AC_SUBST(SEAMTOOL)
   AC_MSG_CHECKING(where to install SEAM extensions)
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

dnl Macro:
dnl   AC_PATH_SEAM (ACTION-IF-FOUND, ACTION-IF-NOT-FOUND)
dnl
dnl Description:
dnl   Check for the presence of the SEAM library and header
dnl   files.
dnl
dnl Author:
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN(AC_PATH_SEAM,
  [ac_seam_ok=""
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

# ---------------------------------------------------------------
# Commonly used macros
# ---------------------------------------------------------------

dnl Macro:
dnl   AC_SEAM_ARG_ENABLE_DEBUGGER
dnl
dnl Description:
dnl   Introduce a build option to build with user-level debugging
dnl   support.
dnl
dnl Authors:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_ARG_ENABLE_DEBUGGER],
  [AC_ARG_ENABLE([debugger],
      AC_HELP_STRING([--enable-debugger],
        [build with user-level debugging support @<:@default=no@:>@]))
   AC_MSG_CHECKING(whether to build with user-level debugging support)
   if test "${enable_debugger:-no}" = "yes"; then
      AC_MSG_RESULT(yes)
      # TODO: Get rid of this message somehow
      AC_MSG_NOTICE([You have chosen to build with user-level debugging. ])
      AC_MSG_NOTICE([Please note that this will only work, if SEAM was   ])
      AC_MSG_NOTICE([also build with user-level debugging support.       ])
      AC_DEFINE(DEBUGGER, 1)
   else
      AC_MSG_RESULT(no)
      AC_DEFINE(DEBUGGER, 0)
   fi])dnl

dnl Macro:
dnl   AC_SEAM_ARG_ENABLE_WARNINGS
dnl
dnl Description:
dnl   Introduce a build option to enable compiler warnings and
dnl   take the necessary options if the option was supplied.
dnl
dnl Authors:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_ARG_ENABLE_WARNINGS],
  [AC_ARG_ENABLE([warnings],
      AC_HELP_STRING([--enable-warnings],
        [enable compiler warnings @<:@default=yes@:>@]))
   AC_MSG_CHECKING(whether to enable compiler warnings)
   if test "${enable_warnings:-yes}" = "yes"; then
      AC_MSG_RESULT(yes)
      AC_SEAM_CHECK_CXXFLAGS(-Wall -W -Wundef -Wpointer-arith -Wcast-qual \
                             -Wcast-align -Wwrite-strings -Wconversion \
                             -Wredundant-decls -Winline \
                             -Woverloaded-virtual -Wsign-promo)
   else
      AC_MSG_RESULT(no)
   fi])dnl

dnl Macro:
dnl   AC_SEAM_CHECK_DECLSPEC
dnl
dnl Description:
dnl   Check whether the compiler understands declspec(dllexport).
dnl
dnl Author:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_CHECK_DECLSPEC],
  [AC_MSG_CHECKING(whether compiler understands __declspec(dllexport))
   AC_LINK_IFELSE(dnl
      AC_LANG_PROGRAM([[void __declspec(dllexport) foo() {};]], []),
     [AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_DLLS, 1)],
     [AC_MSG_RESULT(no)
      AC_DEFINE(HAVE_DLLS, 0)])])dnl

dnl Macro:
dnl   AC_SEAM_CHECK_SOCKET_FLAVOR
dnl
dnl Description:
dnl   Determine the library that provides sockets.
dnl
dnl Author:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_CHECK_SOCKET_FLAVOR],
  [AC_MSG_CHECKING(for Unix select vs. WinSock)
   AC_LINK_IFELSE(dnl
      AC_LANG_PROGRAM(dnl
        [[#include <sys/select.h>]],
        [[select(0, 0, 0, 0, 0);]]),
     [AC_MSG_RESULT(unix)
      AC_DEFINE(USE_WINSOCK, 0)],
     [ac_seam_save_LIBS="${LIBS}"
      LIBS="${LIBS}${LIBS:+ }-lwsock32"
      AC_RUN_IFELSE(dnl
         AC_LANG_PROGRAM(dnl
           [[#include <winsock.h>]],
           [[WSADATA wsa_data;
             WORD req_version = MAKEWORD(1, 1);
             return WSAStartup(req_version, &wsa_data);]]),
        [AC_MSG_RESULT(-lwsock32)
         AC_DEFINE(USE_WINSOCK, 1)],
        [LIBS="wsock32.lib${LIBS:+ }${LIBS}"
         AC_RUN_IFELSE(dnl
            AC_LANG_PROGRAM(dnl
              [[#include <winsock.h>]],
              [[WSADATA wsa_data;
                WORD req_version = MAKEWORD(1, 1);
                return WSAStartup(req_version, &wsa_data);]]),
           [AC_MSG_RESULT(wsock32.lib)
            AC_DEFINE(USE_WINSOCK, 1)],
           [AC_MSG_RESULT(none)
            LIBS="${ac_seam_save_LIBS}"])])])])dnl

dnl Macro:
dnl   AC_SEAM_WITH_LIGHTNING
dnl
dnl Description:
dnl   Introduce a build option for the use of GNU lightning.
dnl
dnl Author:
dnl   Leif Kornstaedt <kornstae@ps.uni-sb.de>
dnl   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
dnl
AC_DEFUN([AC_SEAM_WITH_LIGHTNING],
  [AC_ARG_WITH(lightning,
      AC_HELP_STRING([--with-lightning],
        [use GNU lightning @<:@default=yes@:>@]))
   if test "${with_lightning}" = "no"; then
      AC_DEFINE(HAVE_LIGHTNING, 0)
   else
      # TODO: Do a version check on GNU lightning instead of this message
      AC_MSG_NOTICE([You have chosen to build using GNU lightning.       ])
      AC_MSG_NOTICE([Please note that this will only work, if your local ])
      AC_MSG_NOTICE([version of GNU lightning is the same as the one that])
      AC_MSG_NOTICE([was used when building SEAM.                        ])
      if test "${with_lightning}" != "yes"; then
         CPPFLAGS="${CPPFLAGS}${CPPFLAGS:+ }-I${with_lightning}"
         CPPFLAGS="${CPPFLAGS} -I${with_lightning}/include"
      fi
      AC_CHECK_HEADER(lightning.h,
        [AC_DEFINE(HAVE_LIGHTNING, 1)],
        [AC_MSG_ERROR(cannot find GNU lightning)])
   fi])dnl

# ---------------------------------------------------------------
# End of file
# ---------------------------------------------------------------
