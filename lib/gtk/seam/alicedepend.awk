
BEGIN { FS = "\""; }

function exists(file) {
  return ((getline var < file) > 0);
}

{
  if (match ($0, /^\w*import.*\".*\"\w*$/))
    if (exists($2 ".aml"))
      depends[FILENAME] = depends[FILENAME] " " $2 ".stc"
}

END {
  for (file in depends) {
    file_stc = gensub(/\.aml$/, ".stc", "g", file)
    print file_stc ": " file depends[file]
  }
}
