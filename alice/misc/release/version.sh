#!/usr/bin/env bash

# This script modifies the version number in all files listed in FILES
# to the version number specified by alicetool.
#
# Author: Christian Mueller <cmueller@ps.uni-sb.de>
#

FILES="../../lib/gtk/seam/debian/alice-gtk.info \
../../lib/regex/debian/alice-regex.info \
../../lib/sqlite/debian/alice-sqlite.info \
../../lib/xml/debian/alice-xml.info \
../../misc/debian/debian/alice-runtime.info \
../../vm-seam/debian/alice.info \
../../lib/gecode/debian/alice-gecode.spec \
../../lib/gtk/seam/debian/alice-gtk.spec \
../../lib/regex/debian/alice-regex.spec \
../../lib/sqlite/debian/alice-sqlite.spec \
../../lib/xml/debian/alice-xml.spec \
../../misc/debian/debian/alice-runtime.spec \
../../vm-seam/debian/alice.spec"


VERSION=$(alicetool --package-version)

for f in $FILES
do
  sed "s/Version:[ \t\v\f]*[0-9][0-9]*\.[0-9][0-9]*/Version: "$VERSION"/g" $f > tmpfile
  mv tmpfile $f
done
