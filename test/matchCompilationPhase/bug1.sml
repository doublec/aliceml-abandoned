import
    datatype option
from "x-alice:/lib/Base.ozf"

fun f ((NONE | SOME 0), NONE) = 0
  | f (SOME 1, NONE) = 1
  | f _ = 2
