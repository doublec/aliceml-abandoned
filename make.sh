#!/bin/sh

# Bootstrap the Alice-to-Mozart compiler.

# To install globally:
#   prefix=/opt/stockhausen-devel
# To install locally:
#   prefix=`pwd`/install

if [ "$1" = "-global" ]
then
   prefix=/opt/stockhausen-devel
else
   prefix=`pwd`/install
fi

echo Trying to install Stockhausen to $prefix...

rm -f stoc/stoc-mozart.*
(cd stoc && make) || exit 1
(cd vm-mozart && make) || exit 1
(cd vm-mozart/bootstrap && make PREFIX=$prefix all) || exit 1
(cd vm-mozart/bootstrap && make PREFIX=$prefix install) || exit 1
(cd vm-mozart/bootstrap && make PREFIX=$prefix clean) || exit 1
(cd vm-mozart/bootstrap && make PREFIX=$prefix bootstrap) || exit 1
(cd vm-mozart/bootstrap && make PREFIX=$prefix install) || exit 1
