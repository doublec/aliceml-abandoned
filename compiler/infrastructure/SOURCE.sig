(*
 * A source file.
 *)


signature SOURCE =
  sig

    type source = string
    type pos    = int * int
    type region = pos * pos

    val nowhere:	region
    val over:		region * region -> region
    val between:	region * region -> region

    val regionToString:	region -> string

  end
