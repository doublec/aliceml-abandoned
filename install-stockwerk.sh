#!/bin/sh

# Bootstrap the Alice-to-Stockwerk compiler.

# To install globally:
#   prefix=/opt/stockhausen-devel
# To install locally:
#   prefix=`pwd`/install

if [ "$1" = "-global" ]
then
   prefix=/opt/stockhausen-stockwerk
else
   prefix=`pwd`/install
fi

echo Trying to install Stockhausen on Stockwerk to $prefix...

PLATFORM=x86-linux

OPTS1= # '--dump-phases' # --dump-abstraction-result' # --dump-intermediate'
OPTS2= # '--dump-phases'
OPTS3= # '--dump-phases' # --dump-intermediate'

TIMEDIR=`pwd`/time
export TIMEDIR

LIGHTNING=1

unset STOCKHOME

rm -f bootstrap/alicec-stockwerk.$PLATFORM #bootstrap/alicedep.$PLATFORM
(cd bootstrap && make) || exit 1
(cd support/lightning && ./configure --prefix=`pwd`/build) || exit 1
(cd support/lightning && make all install) || exit 1
(cd vm-stockwerk && make LIGHTNING=${LIGHTNING}) || exit 1
(cd vm-stockwerk && make -f Makefile.bootstrap depend) || exit 1
(cd vm-stockwerk && /usr/bin/time -po ${TIMEDIR}1 make -f Makefile.bootstrap ALICEC_EXTRA_OPTS="$OPTS1" build1-install) || exit 1
(cd vm-stockwerk && make -f Makefile.bootstrap ALICEC_EXTRA_OPTS="$OPTS2" build2-install) || exit 1
(cd vm-stockwerk && /usr/bin/time -po ${TIMEDIR}3 make -f Makefile.bootstrap ALICEC_EXTRA_OPTS="$OPTS3" build3-install) || exit 1
(cd vm-stockwerk && make -f Makefile.bootstrap PREFIX=$prefix install) || exit 1

echo Done.
echo Time for build 1:
cat ${TIMEDIR}1
echo Time for build 3:
cat ${TIMEDIR}3
