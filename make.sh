#!/bin/sh

# Build all files needed to bootstrap the Alice-to-Mozart compiler.

(cd stoc && make) || exit 1
(cd vm-mozart && make) || exit 1
(cd vm-mozart/bootstrap && make) || exit 1
