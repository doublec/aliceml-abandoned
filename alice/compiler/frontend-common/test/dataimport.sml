import datatype option from "dataexport.ozf"

fun f NONE = 0
  | f (SOME x) = f x
