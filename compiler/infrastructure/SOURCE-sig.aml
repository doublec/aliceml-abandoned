(*
 * A source file.
 *)


signature SOURCE =
  sig

    type source   = string
    type pos      = int * int
    type position = pos * pos

    val nowhere:	position
    val over:		position * position -> position
    val between:	position * position -> position

    val positionToString:	position -> string

  end
