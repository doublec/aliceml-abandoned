(* Since SML allows multiple definitions of the same id in a structure,
   labels are not enough for paths. So we added an index. *)

signature PATH =
  sig

  (* Types *)

    type lab   = Label.t
    type name  = Name.t

    type path
    type t = path

  (* Operations *)

    val invent :	unit -> path
    val pervasive :	name -> path
    val fromLab :	lab  -> path
    val toLab :		path -> lab
    val path :		path * lab * int -> path

    val equals :	path * path -> bool
    val compare :	path * path -> order
    val hash :		path -> int

    val isDot :		path -> bool
    val asDot :		path -> path * lab * int

    val strengthen :	path * lab * int * path -> unit

    val clone :		path -> path
    val instance :	('rea * path -> path option) -> 'rea * path -> path

  end
