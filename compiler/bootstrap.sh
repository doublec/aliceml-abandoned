#!/bin/sh

./mksrc 2000 | sml @SMLload=stoc-${1:-mozart}
