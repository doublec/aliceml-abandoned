(*
 * Standard ML label identifiers
 *
 * Definition, section 2.4
 *)


signature LAB =
  sig

    eqtype Lab
    type t = Lab

    val fromString:	string       -> Lab
    val fromInt:	int          -> Lab
    val fromLargeInt:	LargeInt.int -> Lab
    val toString:	Lab          -> string

    val equalsNum:	Lab * LargeInt.int -> bool

    val compare:	Lab * Lab -> order

  end
(*DEBUG*) where type Lab = string
