#!/bin/sh

# Install the non-bootstrapped system, used for testing the libraries.
# This has to be preceded by the following commands from make.sh:
#    rm -f stoc/stoc-mozart.*
#    (cd stoc && make) || exit 1
#    (cd vm-mozart && make) || exit 1


if [$1 = ""]
then
  PREFIX=/opt/stockhausen-devel0
else
  PREFIX=$1
fi

# Cleanup/Create Directories
#rm -rf $PREFIX/bin $PREFIX/lib
mkdir $PREFIX/bin 2>/dev/null
mkdir $PREFIX/lib 2>/dev/null
# Copy Main Files
install -c -m444 stoc/backend-mozart/stoc-mozart.exe $PREFIX/
install -c -m444 stoc/stoc-mozart.x86-linux $PREFIX/
# Copy System Libraries
install -c -m444 vm-mozart/*.ozf $PREFIX/lib
install -c -m444 vm-mozart/bootstrap/lib/*.ozf.sig $PREFIX/lib/
#(cd vm-mozart/bootstrap && make Default.import) || exit 1
install -c -m444 vm-mozart/bootstrap/Base.import $PREFIX/Default.import
# Copy Base Lib
(cd vm-mozart/bootstrap && rm -f lib/Base.ozf && make lib/Base.ozf) || exit 1
install -c -m444 vm-mozart/bootstrap/lib/Base.ozf $PREFIX/lib
install -c -m444 vm-com+/Base.dll.sig $PREFIX/lib/Base.ozf.sig
# Copy Bin Files
install -c -m555 vm-mozart/bootstrap/stoc.test $PREFIX/bin/stoc
install -c -m555 vm-mozart/bootstrap/stow.test $PREFIX/bin/stow
