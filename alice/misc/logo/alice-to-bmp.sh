#!/bin/sh

gs="gs -q -dBATCH -dNOPAUSE -sDEVICE=bmp16m -r300"

process() {
    mpost $1 || exit 1
    $gs -g"$2"x"$3" -sOutputFile="$1"-"$2"x"$3".bmp "$1".1
}

(
    read name xsize ysize
    while [ ! -z "$name" ]
    do
	process "$name" "$xsize" "$ysize"
	read name xsize ysize
    done
) <<EOF
alice-base-plain-black 2096 661
alice-strikethrough-plain-black 2096 661
alice-strikethrough-plain-black 2096 661
alice-strikethrough-shadow-blue 2096 661
alice-strikethrough-shadow-rainbow 2096 661
alice-none-shadow-rainbow 2096 661
alice-ps 2096 661
EOF
