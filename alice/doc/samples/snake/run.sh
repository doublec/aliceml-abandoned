#!/usr/bin/env bash

if [ `uname | head -c 6 | tr [A-Z] [a-z]` = "cygwin" ]; then
  export ALICE_JIT_MODE=0 #little workaround
fi
(make && 
 alicerun client/ClientMain)
