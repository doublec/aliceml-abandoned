structure FixityReflection :> FIXITY_REFLECTION =
  struct

  (* Types *)

    val typ_int		= PervasiveType.typ_int

    val lab_left	= Label.fromString "LEFT"
    val lab_right	= Label.fromString "RIGHT"
    val lab_neither	= Label.fromString "NEITHER"
    val row_assoc	= Type.extendRow(lab_left, PervasiveType.typ_zero,
			  Type.extendRow(lab_right, PervasiveType.typ_zero,
			  Type.extendRow(lab_neither, PervasiveType.typ_zero,
					 Type.emptyRow())))
    val typ_assoc	= Type.inMu(Type.inSum row_assoc)
    datatype assoc	= datatype Fixity.assoc			(* verify *)
    val (LEFT | RIGHT | NEITHER) : assoc = LEFT			(* verify *)

    val lab_nonfix	= Label.fromString "NONFIX"
    val lab_prefix	= Label.fromString "PREFIX"
    val lab_postfix	= Label.fromString "POSTFIX"
    val lab_infix	= Label.fromString "INFIX"
    val typ_intassoc	= Type.inTuple #[PervasiveType.typ_int, typ_assoc]
    val row_fix		= Type.extendRow(lab_nonfix, PervasiveType.typ_zero,
			  Type.extendRow(lab_prefix, PervasiveType.typ_int,
			  Type.extendRow(lab_postfix, PervasiveType.typ_int,
			  Type.extendRow(lab_infix, typ_intassoc,
					 Type.emptyRow()))))
    val typ_fix		= Type.inMu(Type.inSum row_fix)
    datatype fix	= datatype Fixity.fix			(* verify *)
    val (NONFIX | PREFIX(_ : int) | POSTFIX(_ : int) | INFIX(_ : int * assoc))
			= NONFIX : fix				(* verify *)


  (* The module *)

    val modname_fixity	= Name.ExId "Fixity"
    structure Fixity	= Fixity				(* verify *)


  (* Type fields *)

    type lab		= Type.lab

    val typname_fix	= Name.ExId "t"
    type typ		= Fixity.t				(* verify *)
    val typname_assoc	= Name.ExId "assoc"
    type assoc		= Fixity.assoc				(* verify *)

    (* Labels are defined above *)

  end
