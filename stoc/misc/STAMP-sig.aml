(*
 * Stamp generator.
 *)


signature STAMP =
  sig

    eqtype stamp
    type t = stamp

    val new :		unit -> stamp
    val toString :	stamp -> string

    val reset :		unit -> unit

    val compare :	stamp * stamp -> order
    val hash :		stamp -> int

  end
(*DEBUG where type stamp = int*)
