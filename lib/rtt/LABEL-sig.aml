signature LAB =
  sig

    eqtype lab					(* [lab,l] *)
    type t = lab


    (* Operations *)

    val fromString :	string -> lab
    val fromInt :	int    -> lab
    val toString :	lab    -> string

    val compare :	lab * lab -> order
    val hash :		lab -> int

  end
