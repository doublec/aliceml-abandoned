INCLUDES = -I$(top_builddir) -I$(top_srcdir)

EXTRA_PROGRAMS = testfp funcfp
noinst_PROGRAMS = fibit incr printf rpn fib fibdelay
noinst_DATA = fibit.ok incr.ok printf.ok rpn.ok fib.ok fibdelay.ok testfp.ok funcfp.ok
EXTRA_DIST = $(noinst_DATA) run-test

if DISASS
LDADD = $(top_builddir)/opcode/libdisass.a
endif

CFLAGS = -g -O2

funcfp_SOURCES = funcfp.c
testfp_SOURCES = testfp.c
fibit_SOURCES = fibit.c
fibdelay_SOURCES = fibdelay.c
fib_SOURCES = fib.c
incr_SOURCES = incr.c
printf_SOURCES = printf.c
rpn_SOURCES = rpn.c

if REGRESSION_TESTING
TESTS = fib fibit fibdelay incr printf rpn \
	#testfp funcfp
TESTS_ENVIRONMENT=$(srcdir)/run-test
endif
