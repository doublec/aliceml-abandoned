#!/bin/sh
###
### Bootstrap Alice-on-Seam.
###

##
## Configuration Section
##

LIGHTNING=1

OPTS1= # '--dump-phases' # --dump-abstraction-result' # --dump-intermediate'
OPTS2= # '--dump-phases'
OPTS3= # '--dump-phases' # --dump-intermediate'

##
## End of Configuration Section
##

if [ "$1" = "-global" ]
then
   prefix=/opt/stockhausen-seam
else
   prefix=`pwd`/install
fi
echo Trying to install Alice-on-Seam to $prefix...

case `uname -s` in
    CYGWIN*)
	SMLPLATFORM=x86-win32
	SUPPORTPLATFORM=mingw32
	CC="gcc -mno-cygwin"
	;;
    *)
	SMLPLATFORM=x86-linux
	SUPPORTPLATFORM=linux
	LIGHTNINGCONFOPTS=
	CC=gcc
	;;
esac

##
## Build Support Libraries: Lightning
##
SUPPORTDIR=`cd support && pwd`
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

##
## Build Seam
##
(cd vm-stockwerk && make LIGHTNING=${LIGHTNING}) || exit 1

##
## Compile the Bootstrap Compiler with SML/NJ
##
rm -f bootstrap/alicec-seam.$SMLPLATFORM #bootstrap/alicedep.$SMLPLATFORM
(cd bootstrap && make alicec-seam.$SMLPLATFORM alicedep.$SMLPLATFORM) || exit 1

##
## Bootstrap Alice on Seam
##
unset ALICE_HOME
TIMEDIR=`pwd`/time
export TIMEDIR
(cd vm-stockwerk && make -f Makefile.bootstrap depend) || exit 1
(cd vm-stockwerk && /usr/bin/time -po ${TIMEDIR}1 make -f Makefile.bootstrap ALICEC_EXTRA_OPTS="$OPTS1" build1-install) || exit 1
(cd vm-stockwerk && make -f Makefile.bootstrap ALICEC_EXTRA_OPTS="$OPTS2" build2-install) || exit 1
(cd vm-stockwerk && /usr/bin/time -po ${TIMEDIR}3 make -f Makefile.bootstrap ALICEC_EXTRA_OPTS="$OPTS3" build3-install) || exit 1
(cd vm-stockwerk && make -f Makefile.bootstrap PREFIX=$prefix install) || exit 1

##
## Build Libraries
##
PATH=$prefix/bin:$PATH
export PATH
unset ALICE_HOME
(cd lib/distribution && make TARGET=seam depend) || exit 1
(cd lib/distribution && make TARGET=seam all PREFIX=$prefix install) || exit 1

##
## Install documentation
##
(cd doc/manual && make PREFIX=$prefix/doc) || exit 1

##
## Finish
##
echo Done.
echo Time for build 1:
cat ${TIMEDIR}1
echo Time for build 3:
cat ${TIMEDIR}3

exit 0
