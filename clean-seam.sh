#!/bin/sh

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

rm -f bootstrap/alicec-seam.$SMLPLATFORM bootstrap/alicedep.$SMLPLATFORM
(cd bootstrap && make clean) || exit 1
(cd vm-seam && make clean WINDOWS=$WINDOWS) || exit
(cd vm-seam && make -f Makefile.bootstrap distclean) || exit 1
(cd lib/distribution && make TARGET=seam distclean) || exit 1
