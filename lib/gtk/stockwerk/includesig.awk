#
# includesig.awk recursively includes a signature into another.
# To include a signature INCLUDE_THIS-sig.aml, insert the following line:
#
# <<< "INCLUDE_THIS-sig.aml" >>>
#
# Everything of INCLUDE_THIS-sig.aml between the "sig" and the "end" keywords
# are included, except for lines beginning with "(**)".
#

BEGIN { 
  FS = "\""; 
  printlines = 0; 
  if (filename=="") filename = "includesig.awk"
}

{
  if (match ($0, /^\W*sig\W*$/)) { if (body) printlines = 1; else print $0 }
  else 
    if (match ($0, /^\W*end\W*$/)) { if (body) printlines = 0; else print $0 }
    else
      if (match ($0, /^<<<\W*\".*\"\W*>>>$/)) { 
	print "(* begin of " $2 " *)\n"     
	call = ARGV[0] " -f " filename " -v filename=" filename  \
	       " -v body=1 < " $2;
	system(call); 
	print "(* end of " $2 " *)\n"
      }
      else 
	if (match ($0, /^\(\*\*\)\(\*\*\)/))
	  printlines = 1-printlines;
	else
	  if (body == 0 || printlines == 1) 
	    if (!match ($0, /^\(\*\*\)/)) 
	      print $0;
}
