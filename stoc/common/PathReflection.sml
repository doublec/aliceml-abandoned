structure PathReflection :> PATH_REFLECTION =
  struct

  (* Types *)

    val path_path	= Path.pervasive "path"		(*UNFINISHED*)
    val typ_path	= Type.inCon(Type.STAR, Type.CLOSED, path_path)

  (* The module *)

    val modname_path	= Name.ExId "Path"
    structure Path	= Path					(* verify *)

  (* Type fields *)

    val typname_path	= Name.ExId "t"
    type path		= Path.t				(* verify *)

  (* Operations *)

    val lab_invent	= Label.fromString "invent"
    val _		= Path.invent : unit -> path		(* verify *)
    val lab_pervasive	= Label.fromString "pervasive"
    val _		= Path.pervasive : string -> path	(* verify *)
    val lab_fromLab	= Label.fromString "fromLab"
    val _		= Path.fromLab : Label.t -> path	(* verify *)

  end
