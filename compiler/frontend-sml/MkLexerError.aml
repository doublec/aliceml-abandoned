structure LexerError :> LEXER_ERROR =
  struct

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


    val nowhere = (0,0)

    fun error pos_e = raise Error pos_e


    fun toString(UnclosedComment)	= "unclosed comment"
      | toString(InvalidChar c)		= "invalid character `" ^
					  Char.toCString c ^ "'"
      | toString(InvalidString)		= "invalid string constant"
      | toString(IntTooLarge)		= "integer constant too large"
      | toString(WordTooLarge)		= "word constant too large"
      | toString(RealTooLarge)		= "real constant too large"
      | toString(CharLengthInvalid s)	= if s = ""
					  then "empty character constant"
					  else "character constant too long"
      | toString(EscapeCharTooLarge uc)	= (if uc then "unicode" else "ASCII") ^
					  " escape character too large"

  end
