signature LEXER_ERROR =
  sig

    type token

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
    exception EOF of Source.pos -> token

    val nowhere :	Source.pos
    val error :		Source.pos * error -> 'a

    val toString :	error -> string

  end
