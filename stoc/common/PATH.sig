(* Since SML allows multiple definitions of the same id in a structure,
   labels are not enough for paths. So we added an index. *)

signature PATH =
  sig

    type stamp = Stamp.t
    type lab   = Lab.t

    datatype path = PLAIN of stamp * lab * int | DOT of path * lab * int
    type t = path

    type subst = path StampMap.t

    val compare :	path * path -> order
    val hash :		path -> int
    val substitute :	subst * path -> path

  end
