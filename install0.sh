#!/bin/sh

if [$1 = ""]
then
  PREFIX=/opt/stockhausen-devel
else
  PREFIX=$1
fi

# Cleanup/Create Directories
rm -rf $PREFIX/bin $PREFIX/lib
mkdir $PREFIX/bin
mkdir $PREFIX/lib
# Copy Main Files
cp stoc/backend-mozart/stoc-mozart.exe $PREFIX/
cp stoc/stoc-mozart.x86-linux $PREFIX/
# Copy System Libraries
cp vm-mozart/*.ozf $PREFIX/lib
cp vm-mozart/bootstrap/lib/*.ozf.sig $PREFIX/lib/
(cd vm-mozart/bootstrap && make Default.import) || exit 1
cp vm-mozart/bootstrap/Default.import $PREFIX/
# Copy Base Lib
cp vm-mozart/bootstrap/lib/Base.ozf $PREFIX/lib
cp vm-com+/Base.dll.sig $PREFIX/lib/Base.ozf.sig
# Copy Bin Files
cp vm-mozart/bootstrap/stoc.test $PREFIX/bin/stoc
chmod +x $PREFIX/bin/stoc
cp vm-mozart/bootstrap/stow $PREFIX/bin/stow
chmod +x $PREFIX/bin/stow
