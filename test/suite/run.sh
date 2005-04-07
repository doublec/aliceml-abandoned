#!/bin/sh

if [ `uname | head -c 6 | tr [A-Z] [a-z]` = "cygwin" ]; then
  ARG=PLATFORM_WINDOWS
else
  ARG=PLATFORM_UNIX
fi
(make PLATFORM=$ARG $@ && clear && 
 alicerun Main 20.000 false true true)