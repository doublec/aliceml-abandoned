signature LEXER_ERROR =
  sig

    datatype error =
	  UnclosedComment
	| InvalidChar of char
	| InvalidString
	| IntTooLarge
	| WordTooLarge
	| RealTooLarge
	| CharLengthInvalid of string
	| EscapeCharTooLarge of bool

    exception Error of Source.pos * error

    val nowhere :	Source.pos
    val error :		Source.pos * error -> 'a

    val toString :	error -> string

  end
