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
dnl  Test for Programs
dnl
: ${CXXFLAGS=""} dnl we do not want autoconf's default

AC_PROG_MAKE_SET
AC_PROG_CXX
AC_PROG_INSTALL

AC_LANG(C++)
AC_SUBST(INCLUDES)

dnl
dnl  Test For C++ Extensions
dnl
AC_CACHE_CHECK([whether compiler understands __declspec(dllexport)],
  [seam_cv_have_dlls],
  [AC_LINK_IFELSE(AC_LANG_PROGRAM([[void __declspec(dllexport) foo() {};]],[]),
		  seam_cv_have_dlls=yes,
		  seam_cv_have_dlls=no)])

case "$seam_cv_have_dlls" in
    no)
	AC_DEFINE(HAVE_DLLS, 0)
	;;
    yes)
	AC_DEFINE(HAVE_DLLS, 1)
	;;
esac

dnl
dnl  Macros for Testing Compiler and Linker Flags
dnl

# Macro: SEAM_CHECK_CXXFLAG (FLAG, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
#   Check whether FLAG is supported by the C++ compiler.
#   Run the shell commands ACTION-IF-TRUE if it is, ACTION-IF-FALSE otherwise.
#   The default for ACTION-IF-TRUE, if not given, is to append FLAG to
#   `CXXFLAGS'.
AC_DEFUN([SEAM_CHECK_CXXFLAG],
  [AC_MSG_CHECKING(whether $CXX accepts $1)
   seam_save_CXXFLAGS="$CXXFLAGS"
   CXXFLAGS="$CXXFLAGS $1"
   AC_COMPILE_IFELSE(AC_LANG_PROGRAM([],[]),
		     AC_MSG_RESULT(yes)
		     CXXFLAGS="$seam_save_CXXFLAGS"
		     ifelse([$2],,[CXXFLAGS="$CXXFLAGS $1"],[$2]),
		     AC_MSG_RESULT(no)
		     CXXFLAGS="$seam_save_CXXFLAGS"
		     $3)])

# Macro: SEAM_CHECK_CXXFLAGS (FLAG...)
#   For each given flag in the whitespace-separated argument list,
#   test whether it is supported by the C++ compiler.
#   If it is, append it to `CXXFLAGS'.
AC_DEFUN([SEAM_CHECK_CXXFLAGS],
  [for seam_cxxflag in $1; do
       SEAM_CHECK_CXXFLAG($seam_cxxflag)
   done])

# Macro: SEAM_CHECK_LDFLAG (FLAG, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
#   Check whether FLAG is supported by the linker.
#   Run the shell commands ACTION-IF-TRUE if it is, ACTION-IF-FALSE otherwise.
#   The default for ACTION-IF-TRUE, if not given, is to append FLAG to
#   `LDFLAGS'.
AC_DEFUN([SEAM_CHECK_LDFLAG],
  [AC_MSG_CHECKING(whether linker accepts $1)
   seam_save_LDFLAGS="$LDFLAGS"
   LDFLAGS="$LDFLAGS $1"
   AC_LINK_IFELSE(AC_LANG_PROGRAM([],[]),
		  AC_MSG_RESULT(yes)
		  LDFLAGS="$seam_save_LDFLAGS"
		  ifelse([$2],,[LDFLAGS="$LDFLAGS $1"],[$2]),
		  AC_MSG_RESULT(no)
		  LDFLAGS="$seam_save_LDFLAGS"
		  $3)])

dnl  Test for some flags we always want:
SEAM_CHECK_CXXFLAGS([-pipe])
SEAM_CHECK_LDFLAG([[-Wl,--export-dynamic]])

dnl
dnl  Test for Unix Sockets vs. WinSock
dnl
AC_CACHE_CHECK([for Unix select vs. WinSock], [seam_cv_socket_flavor],
  [AC_LINK_IFELSE(AC_LANG_PROGRAM([[#include <sys/select.h>]],
				  [[select(0, NULL, NULL, NULL, NULL);]]),
		  [seam_cv_socket_flavor=unix],
		  [seam_save_LIBS="$LIBS"
		   LIBS="-lwsock32 $LIBS"
		   AC_RUN_IFELSE(AC_LANG_PROGRAM([[#include <winsock.h>]],
		     [[WSADATA wsa_data;
		       WORD req_version = MAKEWORD(1, 1);
		       return WSAStartup(req_version, &wsa_data);]]),
		     [seam_cv_socket_flavor=winsock],
		     [seam_cv_socket_flavor=none])
		   LIBS="$seam_save_LIBS"])])

case "$seam_cv_socket_flavor" in
    unix)
	AC_DEFINE(USE_WINSOCK, 0)
	;;
    winsock)
	AC_DEFINE(USE_WINSOCK, 1)
	LIBS="-lwsock32 $LIBS"
	;;
    none)
	AC_MSG_ERROR([cannot find a usable sockets library])
	;;
esac

dnl
dnl  Test for Memory Allocation Mechanism
dnl
AC_CACHE_CHECK([which memory allocation mechanism to use],
	       [seam_cv_alloc_flavor],
  [AC_LINK_IFELSE(AC_LANG_PROGRAM([[#include <windows.h>]],
				  [[VirtualAlloc(NULL, 0, 0, 0);]]),
		  [seam_cv_alloc_flavor=virtualalloc],
		  [AC_LINK_IFELSE(AC_LANG_PROGRAM([[#include <sys/mman.h>]],
		     [[mmap(NULL, 16,
			    PROT_READ | PROT_WRITE,
			    MAP_PRIVATE | MAP_ANONYMOUS,
			    -1, (off_t) 0) == MAP_FAILED;]]),
		     [seam_cv_alloc_flavor=mmap],
		     [seam_cv_alloc_flavor=malloc])])])

case "$seam_cv_alloc_flavor" in
    virtualalloc)
	AC_DEFINE(HAVE_VIRTUALALLOC, 1)
	;;
    mmap)
	AC_DEFINE(HAVE_VIRTUALALLOC, 0)
	AC_DEFINE(HAVE_MMAP, 1)
	;;
    malloc)
	AC_DEFINE(HAVE_VIRTUALALLOC, 0)
	AC_DEFINE(HAVE_MMAP, 0)
	;;
esac

dnl
dnl  Test for Signal and Friends
dnl
AC_CACHE_CHECK([for signal], [seam_cv_have_signal],
  [AC_LINK_IFELSE(AC_LANG_PROGRAM([[#include <signal.h>]],
				  [[signal(SIGALRM, NULL);]]),
		  [seam_cv_have_signal=yes],
		  [seam_cv_have_signal=no])])

case "$seam_cv_have_signal" in
    no)
	AC_DEFINE(HAVE_SIGNAL, 0)
	;;
    yes)
	AC_DEFINE(HAVE_SIGNAL, 1)
	;;
esac

AC_CACHE_CHECK([for sig_atomic_t], [seam_cv_have_sig_atomic_t],
  [AC_COMPILE_IFELSE(AC_LANG_PROGRAM([[#include <signal.h>]],
				     [[sig_atomic_t x;]]),
		     [seam_cv_have_sig_atomic_t=yes],
		     [seam_cv_have_sig_atomic_t=no])])

case "$seam_cv_have_sig_atomic_t" in
    no)
	AC_DEFINE(HAVE_SIG_ATOMIC_T, 0)
	;;
    yes)
	AC_DEFINE(HAVE_SIG_ATOMIC_T, 1)
	;;
esac

AC_CACHE_CHECK([for SetConsoleCtrlHandler], [seam_cv_have_consolectrl],
  [AC_LINK_IFELSE(AC_LANG_PROGRAM([[#include <windows.h>]],
				  [[SetConsoleCtrlHandler(NULL, TRUE);]]),
		  [seam_cv_have_consolectrl=yes],
		  [seam_cv_have_consolectrl=no])])

case "$seam_cv_have_consolectrl" in
    no)
	AC_DEFINE(HAVE_CONSOLECTRL, 0)
	;;
    yes)
	AC_DEFINE(HAVE_CONSOLECTRL, 1)
	;;
esac

dnl
dnl  Test for dlopen
dnl
AC_CHECK_LIB(dl, dlopen)
AC_CACHE_CHECK([for dlopen], [seam_cv_have_dlopen],
  [AC_LINK_IFELSE(AC_LANG_PROGRAM([[#include <dlfcn.h>]],
				  [[dlopen("foo", RTLD_NOW | RTLD_GLOBAL);]]),
		  seam_cv_have_dlopen=yes,
		  seam_cv_have_dlopen=no)])

case "$seam_cv_have_dlopen" in
    no)
	AC_DEFINE(HAVE_DLOPEN, 0)
	;;
    yes)
	AC_DEFINE(HAVE_DLOPEN, 1)
	;;
esac

dnl
dnl  Test for zlib
dnl
AC_MSG_CHECKING([for zlib])
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
dnl  Test for GNU Lightning
dnl
AC_MSG_CHECKING([for GNU Lightning])
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
dnl  Test for --enable-checked
dnl
AC_MSG_CHECKING([whether to perform a checked build])
AC_ARG_ENABLE([checked],
	      AC_HELP_STRING([--enable-checked],
			     [build with debug symbols and assertions
			      (default: no)]),
	      [seam_enable_checked="$enable_checked"],
	      [seam_enable_checked=no])
AC_MSG_RESULT($seam_enable_checked)

case "$seam_enable_checked" in
    no)
	;;
    yes)
	SEAM_CHECK_CXXFLAGS([-fno-inline-functions -fimplement-inlines])
	SEAM_CHECK_CXXFLAG([-ggdb], , SEAM_CHECK_CXXFLAG([-g]))
	AC_DEFINE(INTERFACE)
	AC_DEFINE(DEBUG_CHECK)
	AC_DEFINE(STORE_DEBUG)
	;;
esac

dnl
dnl  Test for --enable-optimized
dnl
AC_MSG_CHECKING([whether to build with optimization])
AC_ARG_ENABLE([optimized],
	      AC_HELP_STRING([--enable-optimized],
			     [build with optimization
			      (default: yes, unless checked build)]),
	      [seam_enable_optimized="$enable_optimized"],
	      [case "$seam_enable_checked" in
		   no)
		       seam_enable_optimized=yes
		       ;;
		   yes)
		       seam_enable_optimized=no
		       ;;
	       esac])
AC_MSG_RESULT($seam_enable_optimized)

case "$seam_enable_optimized" in
    no)
	;;
    yes)
	dnl--** machine-specific optimizations need more work
	case $(uname -n) in
	    i686)
		SEAM_CHECK_CXXFLAGS([-mcpu=pentium -march=pentium3])
		;;
	esac
	SEAM_CHECK_CXXFLAGS([-O3 -fomit-frame-pointer \
			     -fforce-mem -fforce-addr \
			     -finline-limit=2500 -fno-implement-inlines \
			     -fno-keep-static-consts -fno-implicit-templates \
			     -fno-implicit-inline-templates])
	SEAM_CHECK_LDFLAG([[-Wl,-S]])
	;;
esac

dnl
dnl  Test for --enable-warnings
dnl
AC_MSG_CHECKING([whether to enable compiler warnings])
AC_ARG_ENABLE([warnings],
	      AC_HELP_STRING([--enable-warnings],
			     [enable compiler warnings (default: yes)]),
	      [seam_enable_warnings="$enable_warnings"],
	      [seam_enable_warnings=yes])
AC_MSG_RESULT($seam_enable_warnings)

case "$seam_enable_warnings" in
    no)
	;;
    yes)
	SEAM_CHECK_CXXFLAGS([-Wall -W -Wundef -Wpointer-arith -Wcast-qual \
			     -Wcast-align -Wwrite-strings -Wconversion \
			     -Wredundant-decls -Winline -Woverloaded-virtual \
			     -Wsign-promo])
	;;
esac

dnl
dnl  Test for --enable-profiler
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
