#!/bin/sh

if [ -x /bin/uname ]; then UNAME=/bin/uname
elif [ -x /usr/bin/uname ]; then UNAME=/usr/bin/uname
else UNAME=uname
fi

system="`$UNAME -m` `$UNAME -s` `$UNAME -r`"

case $system in
   *86*Linux*)
	OZARCH=linux-i486
	SMLNJARCH=x86-linux
   ;;
   i[3456]86\ FreeBSD\ *)
        OZARCH=freebsdelf-i486
        SMLNJARCH=x86-bsd
   ;;
   *i[3456]86*CYGWIN*)
	OZARCH=win32-i486
	which sml.bat >/dev/null 2>/dev/null
	if [ $? = 0 ]
	then
	    SMLNJARCH=x86-win32
	else
	    SMLNJARCH=x86-cygwin
	fi
   ;;
   *Power*Mac*Darwin*)
        OZARCH=powermac-darwin
        SMLNJARCH=ppc-darwin
   ;;
   *)
	echo Cannot determine platform type for $system. >&2
	exit 1
   ;;
esac

if [ "$1" = oz ]
then
    echo $OZARCH
elif [ "$1" = smlnj ]
then
    echo $SMLNJARCH
else
    echo 'Usage: platform.sh (oz|smlnj)' >&2
    exit 2
fi
