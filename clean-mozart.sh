#!/bin/sh

rm -f bootstrap/alicec-mozart.$PLATFORM bootstrap/alicedep.$PLATFORM
(cd bootstrap && make clean) || exit 1
(cd vm-mozart && make distclean) || exit 1
(cd lib/inspector && make distclean) || exit 1
(cd lib/constraints && make distclean) || exit 1
(cd lib/distribution && make distclean) || exit 1
(cd lib/gtk && [ -f Makefile ] && make distclean) || exit 1

