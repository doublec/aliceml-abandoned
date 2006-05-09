#!/bin/sh

# This script prepends a new changelog entry to all changelog files
# found in PATHS.
#
# Author: Christian Mueller <cmueller@ps.uni-sb.de>
#

VERSION=$(alicetool --package-version)

PATHS="../../lib/gecode/debian \
../../lib/gtk/seam/debian \
../../lib/regex/debian \
../../lib/sqlite/debian \
../../lib/xml/debian \
../../misc/debian/debian \
../../vm-seam/debian"

DATE=$(date -R)

for path in $PATHS 
do
  file=$path/changelog
  echo "process "$file
  name=$(cd $path; ls *.spec | cut -f1 -d'.')
  cat $file > __tmpfile
  echo $name" ("$VERSION"-1) unstable; urgency=low"  > $file
  echo                                              >> $file
  echo "  * New upstream release"                   >> $file
  echo                                              >> $file
  echo " -- Guido Tack <tack@ps.uni-sb.de>  "$DATE  >> $file
  echo                                              >> $file
  cat __tmpfile >> $file
done

rm __tmpfile
