#!/bin/sh
# Shell script to generate Key.aml 
# calls prepare-keyval.awk to do the actual work
# Copyright: Benedikt Grundmann <bgrund@ps.uni-sb.de>
# Last edited by $Author$ at $Date$


KEYSYM=gdkkeysyms.h
KEYSYMFILE=
# search for the KEYSYM file 
for opt in `pkg-config --cflags gtk+-2.0`; do
  # is opt a -I include option?
  if [[ "-I" == `echo "$opt" | head -c 2` ]]; then
     INCDIR=`echo "$opt" | tail -c +3`
     # does the keysym file exists in the subdirectory gdk?
     if [[ -f "${INCDIR}/gdk/${KEYSYM}" ]]; then
     	# yes it does we found it :-)
	KEYSYMFILE="${INCDIR}/gdk/${KEYSYM}";
	break;
     fi;
  fi;
done;

if [[ -z "${KEYSYMFILE}" ]]; then
   echo "Could not find file gdkkeysyms.h!  Is gtk+-2.0 correctly installed?";
   exit 1;
fi;


gawk -f prepare-keyval.awk "${KEYSYMFILE}"
