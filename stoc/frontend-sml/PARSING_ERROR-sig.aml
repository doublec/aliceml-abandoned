signature PARSING_ERROR =
  sig

    type VId = VId.t

    datatype error =
	(* Lexer *)
	  UnclosedComment
	| InvalidChar		of char
	| InvalidString
	| IntTooLarge
	| WordTooLarge
	| RealTooLarge
	| CharLengthInvalid	of string
	| EscapeCharTooLarge	of bool
	(* Parser *)
	| SyntaxError		of string
	(* Derived forms *)
	| WithtypeInvalid
	| WithtypeArityMismatch

    type warning	(* yet empty *)

    val error :	Source.region * error -> 'a
    val warn :	Source.region * warning -> unit

  end
