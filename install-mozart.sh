#!/bin/sh

# Bootstrap the Alice-to-Mozart compiler.

# To install globally:
#   prefix=/opt/stockhausen-release
# To install locally:
#   prefix=`pwd`/install

if [ "$1" = "-global" ]
then
   prefix=/opt/stockhausen-devel
else
   prefix=`pwd`/install
fi

echo Trying to install Stockhausen to $prefix...

PLATFORM=x86-linux

OPTS1= # '--dump-phases' # --dump-abstraction-result' # --dump-intermediate'
OPTS2= # '--dump-phases'
OPTS3= # '--dump-phases' # --dump-intermediate'

TIMEDIR=`pwd`/time
export TIMEDIR

rm -f bootstrap/alicec-mozart.$PLATFORM #bootstrap/alicedep.$PLATFORM
(cd bootstrap && make) || exit 1
(cd vm-mozart && make depend) || exit 1
(cd vm-mozart && /usr/bin/time -po ${TIMEDIR}1 make ALICEC_EXTRA_OPTS="$OPTS1" build1-install) || exit 1
(cd vm-mozart && make ALICEC_EXTRA_OPTS="$OPTS2" build2-install) || exit 1
(cd vm-mozart && /usr/bin/time -po ${TIMEDIR}3 make ALICEC_EXTRA_OPTS="$OPTS3" build3-install) || exit 1
(cd vm-mozart && make PREFIX=$prefix install) || exit 1
PATH=$prefix/bin:$PATH
export PATH
unset ALICE_HOME
(cd lib/inspector && make depend) || exit 1
(cd lib/inspector && make all PREFIX=$prefix install) || exit 1
(cd lib/constraints && make depend) || exit 1
(cd lib/constraints && make all PREFIX=$prefix install) || exit 1
(cd lib/distribution && make depend) || exit 1
(cd lib/distribution && make all PREFIX=$prefix install) || exit 1
(cd lib/gtk && autoconf && ./configure --with-gtk-canvas-dir=/opt/gtk-canvas) || exit 1
#(cd lib/gtk && make depend) || exit 1
(cd lib/gtk && make all PREFIX=$prefix install) || exit 1
(cd doc/manual && make PREFIX=$prefix/doc) || exit 1

echo Done.
echo Time for build 1:
cat ${TIMEDIR}1
echo Time for build 3:
cat ${TIMEDIR}3
