(* Since SML allows multiple definitions of the same id in a structure,
   labels are not enough for paths. So we added an index. *)

signature PATH =
  sig

  (* Types *)

    type lab   = Lab.t
    type name  = Name.t

    eqtype path
    type t = path

  (* Operations *)

    val invent :	unit -> path
    val fromLab :	lab  -> path
    val toLab :		path -> lab
    val path :		path * lab * int -> path

    val compare :	path * path -> order
    val hash :		path -> int

    val substitute :	path * path * lab * int -> unit
    val realise :	('rea * path -> path option) -> 'rea * path -> unit

    val cloneBinder :	('rea * path -> path option) -> 'rea * path -> path
    val cloneFree :	('rea * path -> path option) -> 'rea * path -> path

  end
