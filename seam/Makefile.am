SUBDIRS = libltdl store adt generic debian

INCLUDES = $(LTDLINCL)

EXTRA_DIST = Makefile.cvs Makefile.lightning.am

# SEAM library

lib_LTLIBRARIES = \
	libseam.la

libseam_la_DEPENDENCIES = \
	$(libseam_la_LIBADD)

libseam_la_SOURCES = \
	Base.cc \
	InitSeam.cc \
	Outline.cc

# used to propagate -no-undefined from configure.ac
libseam_la_LDFLAGS = \
	@EXTRA_LDFLAGS@

libseam_la_LIBADD = \
	$(LIBLTDL) \
	adt/libadt.la \
	generic/libgeneric.la \
	store/libstore.la

# SEAM executable

bin_PROGRAMS = seam

seam_SOURCES = SeamMain.cc
seam_LDADD = libseam.la
seam_DEPENDENCIES = libseam.la

# SEAMTool

bin_SCRIPTS = seamtool

# Header files (including lightning)

nobase_dist_pkginclude_HEADERS = \
	Base.hh \
	Seam.hh \
	libltdl/ltdl.h

# Autoconf macros

aclocaldir = $(datadir)/aclocal

dist_aclocal_DATA = seam.m4
