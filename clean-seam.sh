#!/bin/sh
rm -f bootstrap/alicec-stockwerk.$PLATFORM
(cd bootstrap && make clean) || exit 1
(cd vm-stockwerk && make clean) || exit
(cd vm-stockwerk && make -f Makefile.bootstrap distclean) || exit 1
