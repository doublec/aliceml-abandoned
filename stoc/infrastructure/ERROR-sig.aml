(*
 * Error handling.
 *)


signature ERROR =
  sig

    (* Import *)

    type region = Source.region


    (* Export *)

    exception Error of region * string

    val error :	region * string -> 'a
    val warn :	region * string -> unit

  end
