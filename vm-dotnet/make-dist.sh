#!/bin/sh

URL=http://www.ps.uni-sb.de/stockhausen/lightning/

rm -rf Alice
mkdir Alice
mkdir Alice/doc
mkdir Alice/examples
dist=`(cd Alice && pwd)`

(
    cd ../doc/homepage
    make install

    for i in \
	index laziness futures components modules extensions \
	incompatibilities usage libraries interop examples threads
    do
	wget --output-document=- $URL$i.php3 2> /dev/null |
	sed 's/php3/html/g' > $dist/doc/$i.html
    done

    cp bg.gif $dist/doc
    cp musicemb.jpg $dist/doc
    cp lightning/style.css $dist/doc
)

for i in \
    Alice-README.txt Alice.cs Alice.dll Base.aml Base.dll Base.dll.sig \
    Canvas.cs Canvas.dll Canvas.dll.sig CommandLine.cs CommandLine.dll \
    CommandLine.dll.sig Default.import Dialog.cs Dialog.dll Dialog.dll.sig \
    IO.cs IO.dll IO.dll.sig OS.cs OS.dll OS.dll.sig Skeleton.il \
    TextIO.cs TextIO.dll TextIO.dll.sig Tools.aml Tools.dll Win32.cs \
    Win32.dll stoc-com+.x86-win32 stoc.bat stow.cs stow.exe streams.dll \
    vars.bat
do
    cp $i $dist
done

for i in \
    concfib.aml hamming.aml infer.aml inflist.aml smolka.aml streams.aml
do
    cp examples/$i $dist/examples
done

cp -r SML Alice/SML
