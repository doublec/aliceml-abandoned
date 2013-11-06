#!/bin/zsh

source config.sh
source log.sh

BUILDDATE=`date +'%Y%m%d'`
export BUILDDATE

if [ -e $buildtop ]
then
    echo Directory or file $buildtop already exists,
    echo trying to remove - content was:
    echo
    find $buildtop -ls
    echo
    rm -rf $buildtop
    if [ -e $buildtop ]
    then
	echo Failed to remove $buildtop - giving up
	exit 1
    fi
fi

mkdir $buildtop ||
{
    echo Could not create directory $buildtop - giving up
    exit 1
}

{
    log_start
    nice -5 sh build.sh
    log_end
} 3> $buildtop/email.txt 4> $buildtop/buildlog.html

if [ -s $buildtop/email.txt ]
then
    awk '{
	    if (match ($0, /^\./))
		printf(".%s", $0);
	    else
		print;
	 }' < email.txt |
    /usr/sbin/sendmail $mailnotify
fi

exit 0
