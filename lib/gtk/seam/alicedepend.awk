
BEGIN { FS = "\""; }

function exists(file) {
  return ((getline var < file) > 0);
}

{
  if (match ($0, /^\w*import.*\".*\"\w*$/))
    depends[FILENAME] = depends[FILENAME] " \\\n         " \
      $2 (exists($2 ".asig") ? ".asig" : ".stc")
}

END {
  for (file in depends)
    print (gensub(/\.aml$/, ".stc", "g", file)) ":" depends[file] "\n"
}
