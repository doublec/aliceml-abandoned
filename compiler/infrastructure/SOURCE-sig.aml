(*
 * A source file.
 *)


signature SOURCE =
  sig

    type source
    type pos	= int * int
    type region	= pos * pos
    type t	= source

    val fromString:	string -> source
    val toString:	source -> string

    val nowhere:	region
    val over:		region * region -> region
    val between:	region * region -> region

    val regionToString:	region -> string

  end
