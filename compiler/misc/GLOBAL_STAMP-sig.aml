(*
 * Global stamp generator.
 *)


signature GLOBAL_STAMP =
  sig
    (*include STAMP*)

    eqtype stamp
    type t = stamp

    val new :		unit   -> stamp
    val fromString :	string -> stamp
    val toString :	stamp  -> string

    val reset :		unit -> unit

    val compare :	stamp * stamp -> order
    val hash :		stamp -> int

  end
