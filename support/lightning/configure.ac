dnl Hey Emacs, I want this in -*- autoconf -*- mode, please.

dnl Copyright 2000, 2001, 2002 Free Software Foundation, Inc.
dnl Please see COPYING for a description your rights and responsibilities
dnl with this software.
dnl Process this file with autoconf to produce a configure script.

dnl ----------------------------- HOST SYSTEM -----------------------------------

AC_PREREQ(2.50)
AC_INIT([GNU lightning], 1.0, bonzini@gnu.org, lightning)
AC_CONFIG_SRCDIR([lightning.h.in])

AC_CANONICAL_TARGET
AM_CONFIG_HEADER(lightning.h)
AM_INIT_AUTOMAKE
AM_CONDITIONAL(NO_ACLOCAL, $ACLOCAL --version | fgrep Copyright > /dev/null 2>&1)

# We don't use autoheader.
AUTOHEADER="touch lightning.h.in"

AC_PROG_CC
AC_PROG_CPP
AC_PROG_RANLIB
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PATH_PROG(INSTALL_INFO, install-info, :, $PATH:/sbin)
AC_EXEEXT

AC_CHECK_FUNCS(memcpy)

case "$target_cpu" in
	i?86)	 cpu=i386; AC_DEFINE(LIGHTNING_I386, 1)		;;
	sparc*)	 cpu=sparc; AC_DEFINE(LIGHTNING_SPARC, 1)	;;
	powerpc) cpu=ppc; AC_DEFINE(LIGHTNING_PPC, 1)		;;
	*)	 AC_MSG_ERROR([cpu $target_cpu not supported])	;;
esac

dnl ---------------------------- COMMAND LINE ---------------------------------

AC_ARG_ENABLE( disassembling,
[  --enable-disassembling  make the test programs disassemble the code
			   enabled by default if host != target],
, enable_disassembling=no)

AM_CONDITIONAL(REGRESSION_TESTING, test "$host_cpu" = "$target_cpu")
if test "$host_cpu" != "$target_cpu"; then
  AC_DEFINE(LIGHTNING_CROSS, 1)
  enable_disassembling=yes
fi

if test "$enable_disassembling" != no; then
  AC_DEFINE(LIGHTNING_DISASSEMBLE)
fi
LIBDISASS=""
AM_CONDITIONAL(DISASS, test "$enable_disassembling" != no)
test "$enable_disassembling" != no && LIBDISASS="libdisass.a"

AC_ARG_ENABLE( assertions,
[  --enable-assertions     perform internal consistency checks],
, enable_assertions=no)

if test "$enable_assertions" != no; then
  AC_DEFINE(_ASM_SAFETY, 1)
fi

AM_CONDITIONAL(LIGHTNING_MAIN, :)

dnl --------------------------- PRODUCE OUTPUT --------------------------------

cpu_dir=lightning/$cpu
AC_CONFIG_LINKS(lightning/asm.h:$cpu_dir/asm.h			dnl
                lightning/fp.h:$cpu_dir/fp.h			dnl
                lightning/core.h:$cpu_dir/core.h		dnl
                lightning/funcs.h:$cpu_dir/funcs.h, [],
  [cpu_dir=$cpu_dir])

AC_SUBST(MANTAINER)
AC_SUBST(VERSION)
AC_SUBST(LIBDISASS)

AC_CONFIG_FILES(Makefile doc/Makefile tests/Makefile opcode/Makefile
	lightning/Makefile)

AC_OUTPUT

# A small sanity check
echo "#include <stdio.h>" > confdefs.h		# dummy input file
CPPFLAGS="$CPPFLAGS -I. -I$srcdir"
AC_TRY_COMPILE([#include "lightning.h"], , ,
  AC_MSG_WARN(the compiler that was found could not compile GNU lightning))
