#!/bin/zsh

###
### Output streams:
###
###   3   important error messages (to mail)
###   4   build log (in HTML)
###

source config.sh
source log.sh

abort() {
    log_end_section
    cat 1>&4 <<EOF
<H2>Aborted</H2>
<PRE>
`echo -e $*`
</PRE>
EOF
    cat 1>&3 <<EOF
Subject: Build on $BUILDDATE failed

`echo -e $*`
EOF
    exit 0
}

execute() {
    "$@" 1>&4 2>&4 ||
    abort "The following command failed with exit status $?:\n$*"
}

mozart_get_cvs() {
    log_start_section Retrieving Mozart from CVS
    cd $buildtop
    execute cvs -d :ext:$USER@cvs.mozart-oz.org:/services/mozart/CVS get mozart
    log_end_section
}

mozart_configure() {
    log_start_section Configuring Mozart
    execute mkdir $buildtop/mozart-build
    cd $buildtop/mozart-build
    execute $buildtop/mozart/configure --prefix=$buildtop/mozart-install
    log_end_section
}

mozart_build() {
    log_start_section Building Mozart
    cd $buildtop/mozart-build
    execute make bootstrap install
    log_end_section
}

alice_get_cvs() {
    log_start_section Retrieving Alice from CVS
    cd $buildtop
    execute cvs -d :ext:$USER@cvs.ps.uni-sb.de:/services/ps/CVS get alice
    log_end_section
}

trap 'abort Got signal' 1 2 15

mozart_get_cvs
mozart_configure
mozart_build

alice_get_cvs

exit 0
