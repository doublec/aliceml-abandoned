# Gawk script to convert gdkkeysyms.h #defines into keyval datatype declaration
# written by Benedikt Grundmann <bgrund@ps.uni-sb.de>
# Last change by $author$ at $date$

BEGIN {
    # main file
    F="Key.aml"
    # temporary file to write keyvalToInt to
    F2="keyvalToInt.tmp.aml"
    # temporary file to write intToKeyval to
    F3="intToKeyval.tmp.aml"
    N=0
    print "(* This file is generated automatically do not edit! *)" > F
    print "structure Key =\nstruct\n   datatype keyval =" >> F

    print "\n    fun keyvalToInt kv =\n        case kv of" > F2
    print "\n    fun intToKeyval i  =\n        case i of" > F3
}

$1 == "#define"	&& $2 ~ /^GDK_/ && $3 ~ /^0x/ { 
	CON="K" substr ($2, 4) 
	VAL=$3
	if (N == 0) {
	   printf "        " >> F
	   printf "            " >> F2
	   printf "            " >> F3
	} else {
	   printf "      | " >> F
	   printf "          | " >> F2
	   printf "          | " >> F3
	}
	print CON >> F
	print CON "\t => " VAL >> F2
	print VAL "\t => " CON >> F3
	N=N + 1
   }

END {
    print "\nend" >> F3
    system ("cat " F2 " " F3 " >> " F "; rm " F2 " " F3)
}
