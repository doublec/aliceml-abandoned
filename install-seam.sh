#!/bin/sh
###
### Bootstrap Alice-on-Seam.
###

##
## Configuration Section
##

OPTS1= # '--dump-phases' # --dump-abstraction-result' # --dump-intermediate'
OPTS2= # '--dump-phases'
OPTS3= # '--dump-phases' # --dump-intermediate'

##
## End of Configuration Section
##

: ${prefix="$(pwd)/install"}

case `uname -s` in
    CYGWIN*)
	WINDOWS=1
	SMLPLATFORM=x86-win32
	;;
    *)
	WINDOWS=0
	SMLPLATFORM=x86-linux
	;;
esac

echo Trying to install Alice-on-Seam to $prefix...

##
## Build Seam
##
(cd vm-seam && ./configure --prefix=${prefix} \
                           --with-lightning="$(pwd)/../../seam-support/install" \
    && make install) || exit 1

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
(cd vm-seam && make -f Makefile.bootstrap depend) || exit 1
(cd vm-seam && /usr/bin/time -po ${TIMEDIR}1 make -f Makefile.bootstrap ALICEC_EXTRA_OPTS="$OPTS1" build1-install) || exit 1
(cd vm-seam && make -f Makefile.bootstrap ALICEC_EXTRA_OPTS="$OPTS2" build2-install) || exit 1
(cd vm-seam && /usr/bin/time -po ${TIMEDIR}3 make -f Makefile.bootstrap ALICEC_EXTRA_OPTS="$OPTS3" build3-install) || exit 1
(cd vm-seam && make -f Makefile.bootstrap PREFIX=$prefix install) || exit 1

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
