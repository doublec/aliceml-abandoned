#!/bin/sh

LIGHTNING=1
SUPPORTDIR="$(pwd)"

##
## Build Support Libraries: Lightning
##
if [ "$LIGHTNING" -ne 0 ]
then
    if [ ! -f "$SUPPORTDIR/install/$SUPPORTPLATFORM/include/lightning.h" ]
    then
	mkdir -p "$SUPPORTDIR/build/$SUPPORTPLATFORM/lightning" 2>/dev/null
	(
	    cd "$SUPPORTDIR/build/$SUPPORTPLATFORM/lightning" &&
	    CC=$CC "$SUPPORTDIR/lightning/configure" \
		--prefix="$SUPPORTDIR/install/$SUPPORTPLATFORM" &&
	    make all install
	) || exit 1
    fi
fi

##
## Build Support Libraries: zlib
##
if [ ! -f "$SUPPORTDIR/install/$SUPPORTPLATFORM/include/zlib.h" ]
then
    (
	cd "$SUPPORTDIR/zlib" &&
	CC=$CC ./configure --prefix="$SUPPORTDIR/install/$SUPPORTPLATFORM" &&
	make all install distclean
    ) || exit 1
fi
