#!/bin/sh

# Bootstrap the Alice-to-Mozart compiler.

# To install globally:
#   prefix=/opt/stockhausen-devel
# To install locally:
#   prefix=`pwd`/install
# Or set prefix manually on invocation

MAKE=make

if [ "$prefix" = "" ]
then
  if [ "$1" = "-global" ]
  then
     prefix=/opt/stockhausen-devel
  else
     prefix=`pwd`/install
  fi
fi

if [ "$PLATFORM" = "" ]
then
  PLATFORM=`bootstrap/platform.sh smlnj`
fi

echo Trying to install Alice to $prefix for platform $PLATFORM...

OPTS1= # '--dump-phases' # --dump-abstraction-result' # --dump-intermediate'
OPTS2= # '--dump-phases'
OPTS3= # '--dump-phases' # --dump-intermediate'

TIMEDIR=`pwd`/time
export TIMEDIR

rm -f bootstrap/alicec-mozart.$PLATFORM #bootstrap/alicedep.$PLATFORM
(cd bootstrap && $MAKE) || exit 1
(cd vm-mozart && $MAKE depend) || exit 1
(cd vm-mozart && /usr/bin/time -po ${TIMEDIR}1 $MAKE ALICEC_EXTRA_OPTS="$OPTS1" build1-install) || exit 1
(cd vm-mozart && $MAKE ALICEC_EXTRA_OPTS="$OPTS2" build2-install) || exit 1
(cd vm-mozart && /usr/bin/time -po ${TIMEDIR}3 $MAKE ALICEC_EXTRA_OPTS="$OPTS3" build3-install) || exit 1
(cd vm-mozart && $MAKE PREFIX=$prefix install) || exit 1
PATH=$prefix/bin:$PATH
export PATH
unset ALICE_HOME
(cd lib/inspector && $MAKE depend) || exit 1
(cd lib/inspector && $MAKE all PREFIX=$prefix install) || exit 1
(cd lib/constraints && $MAKE depend) || exit 1
(cd lib/constraints && $MAKE all PREFIX=$prefix install) || exit 1
(cd lib/distribution && $MAKE depend) || exit 1
(cd lib/distribution && $MAKE all PREFIX=$prefix install) || exit 1
(cd lib/gtk && autoconf && ./configure --with-gtk-canvas-dir=/opt/gtk-canvas) || exit 1
#(cd lib/gtk && $MAKE depend) || exit 1
(cd lib/gtk && $MAKE all PREFIX=$prefix install) || exit 1
(cd doc/manual && $MAKE PREFIX=$prefix/doc) || exit 1

echo Done.
echo Time for build 1:
cat ${TIMEDIR}1
echo Time for build 3:
cat ${TIMEDIR}3
