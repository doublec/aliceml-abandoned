#!/bin/sh

rm -f bootstrap/alicec-mozart.$PLATFORM bootstrap/alicedep.$PLATFORM
(cd bootstrap && make clean) || exit 1
(cd vm-mozart && make clean) || exit 1
(cd lib/inspector && make clean) || exit 1
(cd lib/constraints && make clean) || exit 1
(cd lib/distribution && make clean) || exit 1
(cd lib/gtk && make celan) || exit 1
