import
    infixr 5 ::
    datatype list
from "x-alice:/lib/Base.ozf"

fun f ((#"/" | #"\\")::(#"/" | #"\\")::rest) = rest
  | f ((#"/" | #"\\")::rest) = rest
  | f x = x
