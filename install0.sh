#!/bin/sh

# Install the non-bootstrapped system, used for testing the libraries.

if [$1 = ""]
then
  PREFIX=/opt/stockhausen-devel0
else
  PREFIX=$1
fi

PLATFORM=x86-linux

#(cd bootstrap &&
# rm -f stoc-mozart.$PLATFORM &&
# make stoc-mozart.exe stoc-mozart.$PLATFORM) || exit 1

LIBS="
	build1/lib/fundamental/Fundamental.ozf
	build1/lib/system/IO.asig
	build1/lib/system/IO.ozf
	build1/lib/system/TextIO.asig
	build1/lib/system/TextIO.ozf
	build1/lib/system/OS.asig
	build1/lib/system/OS.ozf
	build1/lib/system/Unix.asig
	build1/lib/system/Unix.ozf
	build1/lib/system/CommandLine.asig
	build1/lib/system/CommandLine.ozf
	build1/lib/system/Tools.asig
	build1/lib/system/Tools.ozf
	build1/lib/system/UnsafePickle.asig
	build1/lib/system/UnsafePickle.ozf
	build1/lib/system/Debug.asig
	build1/lib/system/Debug.ozf
	build1/lib/system/Socket.asig
	build1/lib/system/Socket.ozf
"

(cd vm-mozart &&
 make depend $LIBS) || exit 1

ozl vm-mozart/build1/lib/fundamental/Fundamental.ozf \
    -o vm-mozart/build1/lib/fundamental/LinkedFundamental.ozf

# Create Directories
mkdir $PREFIX 2>/dev/null
mkdir $PREFIX/bin 2>/dev/null
mkdir $PREFIX/lib 2>/dev/null
mkdir $PREFIX/lib/fundamental 2>/dev/null
mkdir $PREFIX/lib/system 2>/dev/null
# Copy Top Files
install -c -m444 bootstrap/stoc-mozart.exe $PREFIX/
install -c -m444 bootstrap/stoc-mozart.x86-linux $PREFIX/
# Copy Bin Files
install -c -m555 vm-mozart/stoc.test $PREFIX/bin/stoc
install -c -m555 vm-mozart/stow.test $PREFIX/bin/stow
# Copy System Libraries
install -c -m444 lib/fundamental/Fundamental.import $PREFIX/Default.import
install -c -m444 \
	vm-mozart/build1/lib/fundamental/LinkedFundamental.ozf \
	$PREFIX/lib/fundamental/Fundamental.ozf
install -c -m444 vm-com+/Base.asig $PREFIX/lib/fundamental/Fundamental.asig
install -c -m444 vm-mozart/build1/lib/system/*.ozf $PREFIX/lib/system
install -c -m444 vm-mozart/build1/lib/system/*.asig $PREFIX/lib/system
