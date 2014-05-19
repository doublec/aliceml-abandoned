#!/usr/bin/env bash

set -e

AUTOMAKE="yes"
: ${LIGHTNING=1}
: ${BUILD_GMP=0}
: ${BUILD_SQLITE=0}
: ${BUILD_LIBXML=0}
SUPPORTDIR="$(pwd)"

: ${prefix="$SUPPORTDIR/install"}

case `uname -sm` in
    *Darwin*)
	CC=gcc-4.2
	LIBTOOLIZE=glibtoolize
	;;
    *CYGWIN*)
	BUILD_GMP=1
	BUILD_SQLITE=1
	BUILD_LIBXML=1
	CC="i686-pc-mingw32-gcc"
	LIBTOOLIZE=libtoolize
	;;
	*x86_64*)
	LIGHTNING=0
	CC=gcc
	LIBTOOLIZE=libtoolize
	;;
    *)
	CC=gcc
	;;
esac

	
##
## Build Support Libraries: Automake/SEAM
##

## Automake version to use
amversion="1.7.6"

if [ "x${AUTOMAKE}" != "x" ]; then
    if test -d "${SUPPORTDIR}/automake-seam/automake-${amversion}"; then
	echo "### Automake/SEAM has been configured already." >&2
	echo "### Please delete the following directory to re-configure it:" >&2
	echo "### ${SUPPORTDIR}/automake-seam/automake-${amversion}" >&2
    else
	echo "### Building Automake/SEAM" >&2
	cd "${SUPPORTDIR}/automake-seam" && (
	    echo "### - extracting the source" >&2
	    tar xzf automake-${amversion}.tar.gz
	    cd automake-${amversion} && (
		echo "### - applying SEAM-specific patches" >&2
		patch -p1 < ../automake-${amversion}.seam.patch
		patch -p1 < ../automake2automake-seam.patch
		echo "### - applying libtool 2.4+ compatibility hacks" >&2
		rm -f aclocal.m4 configure
		find . -name Makefile.in | xargs rm -f
		echo "### - reconfiguring the source" >&2
		$LIBTOOLIZE --automake && aclocal && automake --add-missing && autoconf &&
		./configure --prefix="${prefix}" &&
		echo "### - building and installing" >&2
		make all install
		echo "### - linking local and global ac dir" >&2
		if which aclocal; then
		    aclocal --print-ac-dir > "${prefix}/share/aclocal-1.7/dirlist"
		fi
	    )
	)
    fi
fi

##
## Build Support Libraries: Lightning
##

# lightning version to use
lightningversion=1.2pre

if [ "$LIGHTNING" -ne 0 ]
then
    if test -d "${SUPPORTDIR}/lightning/lightning-${lightningversion}"; then
	echo "### Lightning/SEAM has been configured already." >&2
	echo "### Please delete the following directory to re-configure it:" >&2
	echo "### ${SUPPORTDIR}/lightning/lightning-${lightningversion}" >&2
    else
	echo "### Building lightning/SEAM" >&2
	cd "${SUPPORTDIR}/lightning" && (
	    echo "### - extracting the source" >&2
	    tar xzf lightning-${lightningversion}.tar.gz
	    cd lightning-${lightningversion} && (
                if test -f "../lightning-${lightningversion}.seam.patch"; then
		  echo "### - applying SEAM-specific patches" >&2
		  patch -p1 < ../lightning-${lightningversion}.seam.patch
                fi
		echo "### - reconfiguring the source" >&2
		mkdir -p "${SUPPORTDIR}/build/lightning" 2>/dev/null
		cd "${SUPPORTDIR}/build/lightning" &&
		"${SUPPORTDIR}/lightning/lightning-${lightningversion}/configure" CC="$CC" --prefix="$prefix" &&
		echo "### - building and installing" >&2 &&
		make all install
	    )
	)
    fi
fi

##
## Build Support Libraries: zlib
##
if [ ! -f "$prefix/include/zlib.h" ]
then
    (
	echo "### Building zlib" >&2
	cd "$SUPPORTDIR/zlib" &&
	echo "### - reconfiguring the source" >&2 &&
	CC="$CC" ./configure --prefix="$prefix" &&
	echo "### - building and installing" >&2 &&
	make all install distclean
    ) || exit 1
fi

##
## Build Support Libraries: gmp
##
# gmp version to use
gmpversion=4.1.3

if [ "$BUILD_GMP" -ne 0 ]
then
if [ ! -f "$prefix/include/gmp.h" ]
then
    (
	echo "### Building gmp" >&2
        cd "$SUPPORTDIR/gmp" &&
	echo "### - extracting the source" >&2 &&
	tar xzf gmp-${gmpversion}.tar.gz &&
	echo "### - reconfiguring the source" >&2 &&
	mkdir -p "${SUPPORTDIR}/build/gmp" 2>/dev/null &&
	cd "${SUPPORTDIR}/build/gmp" &&
	"${SUPPORTDIR}/gmp/gmp-${gmpversion}/configure" --build i586 CC="$CC" --prefix="$prefix" &&
	echo "### - building and installing" >&2 &&
	make all install
    ) || exit 1
fi
fi

##
## Build Support Libraries: sqlite
##
# sqlite version to use
sqliteversion=3.1.3

if [ "$BUILD_SQLITE" -ne 0 ]
then
if [ ! -f "$prefix/include/sqlite3.h" ]
then
    (
	echo "### Building sqlite" >&2
        cd "$SUPPORTDIR/sqlite" &&
	echo "### - extracting the source" >&2 &&
	tar xzf sqlite-${sqliteversion}.tar.gz &&
	(cd sqlite-${sqliteversion} &&
	 echo "### - applying patches" >&2 &&
	 patch -p1 < ../sqlite-${sqliteversion}.patch) &&
	echo "### - reconfiguring the source" >&2 &&
	mkdir -p "${SUPPORTDIR}/build/sqlite" 2>/dev/null &&
	cd "${SUPPORTDIR}/build/sqlite" &&
	"${SUPPORTDIR}/sqlite/sqlite-${sqliteversion}/configure" CC="$CC" --prefix="$prefix" --disable-tcl --disable-shared --enable-static &&
	echo "### - building and installing" >&2 &&
	make all install
    ) || exit 1
fi
fi

##
## Build Support Libraries: libxml2
##
# libxml version to use
libxml2version=2.6.17

if [ "$BUILD_LIBXML" -ne 0 ]
then
if [ ! -f "$prefix/include/libxml2/libxml/parser.h" ]
then
    (
	echo "### Building libxml2" >&2
        cd "$SUPPORTDIR/libxml2" &&
	echo "### - extracting the source" >&2 &&
	tar xzf libxml2-${libxml2version}.tar.gz &&
	echo "### - reconfiguring the source" >&2 &&
	cd "${SUPPORTDIR}/libxml2/libxml2-${libxml2version}" &&
	"./configure" \
        CC="$CC" CXX="$CXX" --prefix="$prefix" \
	--with-minimum --with-sax1 --with-tree --with-output &&
	echo "### - building and installing" >&2 &&
	make all install
    ) || exit 1
fi
fi

##
## Clean up aclocal path mess
##

echo "### Clean up aclocal path mess" >&2
mkdir -p "$prefix"/share/aclocal
cp "$prefix"/share/aclocal-1.7/* "$prefix"/share/aclocal/


