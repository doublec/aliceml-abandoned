infixr 5 ::
datatype 'a list = :: of 'a * 'a list | nil

fun f ((#"/" | #"\\")::(#"/" | #"\\")::rest) = rest
  | f ((#"/" | #"\\")::rest) = rest
  | f x = x
