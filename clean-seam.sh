#!/bin/sh

rm -f bootstrap/alicec-seam.$PLATFORM bootstrap/alicedep.$PLATFORM
(cd bootstrap && make clean) || exit 1
(cd vm-stockwerk && make clean) || exit
(cd vm-stockwerk && make -f Makefile.bootstrap distclean) || exit 1
(cd lib/distribution && make TARGET=seam distclean) || exit 1
