#!/bin/sh

for i in $*
do
    echo '(*' $i '*)'
    awk '/^###/ { system ("cat ../../stoc/" substr($0, 4)) }
	 /^##-/ { system ("cat " substr($0, 4)) }
	 /^[^#]|^$/ { print }' < $i
done
