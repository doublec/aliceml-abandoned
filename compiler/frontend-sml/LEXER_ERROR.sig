signature LEXER_ERROR =
  sig

    type token
    type error

    exception Error of (int * int) * error
    exception EOF   of (int * int) -> token

    val error :	(int * int) * error -> 'a

  end
