functor LexerError(structure Tokens: Parser_TOKENS
		   type error) : LEXER_ERROR =
  struct

    type token = (Tokens.svalue, int) Tokens.token
    type error = error

    exception Error of (int * int) * error
    exception EOF   of (int * int) -> token

    fun error pos_e = raise Error pos_e

  end
