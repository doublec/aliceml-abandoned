structure PervasiveTypeReflection :> PERVASIVE_TYPE_REFLECTION =
  struct

  (* The module *)

    val modname_pervasiveType	= Name.ExId "PervasiveType"
    structure PervasiveType	= PervasiveType			(* verify *)


  (* Operations *)

    type typ = Type.t
    type con = Type.con

    val lab_typ_ref	= Label.fromString "typ_ref"
    val _		= PervasiveType.typ_ref : typ		(* verify *)
    val lab_lookup	= Label.fromString "lookup"
    val _		= PervasiveType.lookup : string -> con	(* verify *)

  end
