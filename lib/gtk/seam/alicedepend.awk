
BEGIN { 
  FS = "\""; 
  if (filename=="") filename = "alicedepend.awk";
  if (outfile=="") outfile = "Makefile.depend";
}

function exists(file) {
  return ((getline var < file) > 0);
}

{
  me = FILENAME;
  if (match ($0, /^\w*import.*\".*\"\w*$/)) {
    newfile = $2 ".aml"
    newfilesep = "-" newfile "-";
    if (exists(newfile) &&
	index (processed, newfilesep) == 0) { # not yet processed?
      if (sublevel) print newfilesep;
      processed = processed newfilesep;
      depends = depends " " newfile;
      call = ARGV[0] " -f " filename " -v sublevel=1 -v filename=" filename \
	     " -v outfile=" outfile " -v processed=" processed " " newfile;
      while ((call |& getline line) > 0)
	processed = processed line;
      close (call);
    }
  }
}

END {
  me_stc = gensub(/\.aml$/, ".stc", "g", me);
  print me_stc ": " me depends >> outfile;
  fflush(outfile);
}
