datatype 'a option = NONE | SOME of 'a

fun f ((NONE | SOME 0), NONE) = 0
  | f (SOME 1, NONE) = 1
  | f _ = 2
