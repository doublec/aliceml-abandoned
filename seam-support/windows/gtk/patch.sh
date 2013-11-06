#!/bin/sh

PWD=$(pwd)

echo "ersetze alle Dateien in denen der String \
/home/pslab/Projects/WinGtk-2.2 vorkommt durch $(PWD)"

SED_EXPRESSION='s#prefix=.*#prefix='$PWD'#'

for i in `find $PWD/lib/pkgconfig -name '*.pc*'`; do
	sed -r -n -e $SED_EXPRESSION -e "w $i.tmp" $i
	echo "$i"
	mv "$i.tmp" "$i"
done

# these need to be executable, but they arent by default
find . -name '*.exe' | xargs chmod +x
find . -name '*.dll' | xargs chmod +x
# the system pkg-config should be used instead of this one (its buggy)
chmod -x 'bin/pkg-config.exe'


