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

PLATFORM=x86-linux

rm -f bootstrap/stoc-mozart.$PLATFORM bootstrap/stodep.$PLATFORM
(cd bootstrap && make) || exit 1
(cd vm-mozart/bootstrap && make depend) || exit 1
(cd vm-mozart/bootstrap && make build3-install) || exit 1
(cd vm-mozart/bootstrap && make PREFIX=$prefix install) || exit 1
