(*
 * Error handling.
 *)


signature ERROR =
  sig

    (* Import *)

    type position = Source.position


    (* Export *)

    exception ERROR of position * string

    val error: position * string -> 'a

  end
