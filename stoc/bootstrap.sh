#!/bin/sh

./mksrc | sml @SMLload=stoc-${1:-mozart} --dryrun
