signature LABEL =
  sig

    eqtype lab					(* [lab,l] *)
    type t = lab


    (* Operations *)

    val fromString :	string -> lab
    val fromInt :	int    -> lab
    val fromName :	Name.t -> lab
    val toName :	lab    -> Name.t
    val toString :	lab    -> string

    val compare :	lab * lab -> order
    val hash :		lab -> int

  end
