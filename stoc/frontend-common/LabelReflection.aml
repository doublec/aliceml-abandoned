structure LabelReflection :> LABEL_REFLECTION =
  struct

  (* Types *)

    val path_lab	= Path.pervasive "lab"		(*UNFINISHED*)
    val typ_lab		= Type.inCon(Type.STAR, Type.CLOSED, path_lab)

  (* The module *)

    val modname_label	= Name.ExId "Label"
    structure Label	= Label					(* verify *)

  (* Type fields *)

    val typname_lab	= Name.ExId "t"
    type lab		= Label.t				(* verify *)

  (* Operations *)

    val lab_fromString	= Label.fromString "fromString"
    val _		= Label.fromString : string -> lab	(* verify *)

  end
