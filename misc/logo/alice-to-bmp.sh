#!/bin/sh

mpost alice || exit 1

gs="gs -q -dBATCH -dNOPAUSE -sDEVICE=bmp16m -r300"

$gs -g2096x661 -sOutputFile=alice-strikethrough-shadow-rainbow.bmp alice.1
