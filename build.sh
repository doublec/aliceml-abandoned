#!/bin/sh

LIGHTNING=1
SUPPORTDIR="$(pwd)"
: ${prefix="$SUPPORTDIR/install"}

case `uname -s` in
    CYGWIN*)
	CC="gcc -mno-cygwin"
	;;
    *)
	CC=gcc
	;;
esac

##
## Build Support Libraries: Lightning
##
if [ "$LIGHTNING" -ne 0 ]
then
    if [ ! -f "$prefix/include/lightning.h" ]
    then
	mkdir -p "$SUPPORTDIR/build/lightning" 2>/dev/null
	(
	    cd "$SUPPORTDIR/build/lightning" &&
	    "$SUPPORTDIR/lightning/configure" CC="$CC" --prefix="$prefix" &&
	    make all install
	) || exit 1
    fi
fi

##
## Build Support Libraries: zlib
##
if [ ! -f "$prefix/include/zlib.h" ]
then
    (
	cd "$SUPPORTDIR/zlib" &&
	CC="$CC" ./configure --prefix="$prefix" &&
	make all install distclean
    ) || exit 1
fi
