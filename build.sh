#!/bin/sh

LIGHTNING=1
SUPPORTDIR="$(pwd)"

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
    if [ ! -f "$SUPPORTDIR/install/include/lightning.h" ]
    then
	mkdir -p "$SUPPORTDIR/build/lightning" 2>/dev/null
	(
	    cd "$SUPPORTDIR/build/lightning" &&
	    CC=$CC "$SUPPORTDIR/lightning/configure" \
		--prefix="$SUPPORTDIR/install" &&
	    make all install
	) || exit 1
    fi
fi

##
## Build Support Libraries: zlib
##
if [ ! -f "$SUPPORTDIR/install/include/zlib.h" ]
then
    (
	cd "$SUPPORTDIR/zlib" &&
	CC=$CC ./configure --prefix="$SUPPORTDIR/install" &&
	make all install distclean
    ) || exit 1
fi
