structure InfReflection :> INF_REFLECTION =
  struct

  (* Types *)

    val typ_path	= PathReflection.typ_path

    val path_sign	= Path.pervasive "sign"		(*UNFINISHED*)
    val typ_sign	= Type.inCon(Type.STAR, Type.CLOSED, path_sign)
    val path_inf	= Path.pervasive "inf"		(*UNFINISHED*)
    val typ_inf		= Type.inCon(Type.STAR, Type.CLOSED, path_inf)
    val path_kind	= Path.pervasive "kind"		(*UNFINISHED*)
    val typ_kind	= Type.inCon(Type.STAR, Type.CLOSED, path_kind)

    val typ_con		= Type.inTuple #[typ_kind, typ_path]
    type con		= Inf.con				(* verify *)
    val _		= (Inf.inGround(), Path.invent()) : con	(* verify *)


  (* The module *)

    val modname_inf	= Name.ExId "Inf"
    structure Inf	= Inf					(* verify *)


  (* Type fields *)

    val typname_kind	= Name.ExId "kind"
    type kind		= Inf.kind				(* verify *)
    val typname_inf	= Name.ExId "t"
    type inf		= Inf.t					(* verify *)
    val typname_sign	= Name.ExId "sign"
    type sign		= Inf.sign				(* verify *)


  (* Operations *)

    type fix		= Fixity.t
    type lab		= Label.t
    type path		= Path.t
    type typ		= Type.t
    type tkind		= Type.kind

    val lab_empty	= Label.fromString "empty"
    val _		= Inf.empty : unit -> sign		(* verify *)

    val lab_newVal	= Label.fromString "newVal"
    val _		= Inf.newVal : sign * lab -> path	(* verify *)
    val lab_newTyp	= Label.fromString "newTyp"
    val _		= Inf.newTyp : sign * lab -> path	(* verify *)
    val lab_newMod	= Label.fromString "newMod"
    val _		= Inf.newMod : sign * lab -> path	(* verify *)
    val lab_newInf	= Label.fromString "newInf"
    val _		= Inf.newInf : sign * lab -> path	(* verify *)
    val lab_newFix	= Label.fromString "newFix"
    val _		= Inf.newFix : sign * lab -> path	(* verify *)

    val lab_extendVal	= Label.fromString "extendVal"	
    val _ = Inf.extendVal : sign * path * typ * path option -> unit (* verify *)
    val lab_extendTyp	= Label.fromString "extendTyp"
    val _ = Inf.extendTyp : sign * path * tkind * typ option -> unit(* verify *)
    val lab_extendMod	= Label.fromString "extendMod"
    val _ = Inf.extendMod : sign * path * inf * path option -> unit (* verify *)
    val lab_extendInf	= Label.fromString "extendInf"
    val _ = Inf.extendInf : sign * path * kind * inf option -> unit (* verify *)
    val lab_extendFix	= Label.fromString "extendFix"
    val _ = Inf.extendFix : sign * path * fix -> unit               (* verify *)

    val lab_inTop	= Label.fromString "inTop"
    val _		= Inf.inTop : unit -> inf		(* verify *)
    val lab_inCon	= Label.fromString "inCon"
    val _		= Inf.inCon : con -> inf		(* verify *)
    val lab_inSig	= Label.fromString "inSig"
    val _		= Inf.inSig : sign -> inf		(* verify *)
    val lab_inArrow	= Label.fromString "inArrow"
    val _		= Inf.inArrow : path * inf * inf -> inf	(* verify *)
    val lab_inLambda	= Label.fromString "inLambda"
    val _		= Inf.inLambda : path * inf * inf -> inf (* verify *)
    val lab_inApply	= Label.fromString "inApply"
    val _		= Inf.inApply : inf * path * inf -> inf	(* verify *)

    val lab_inGround	= Label.fromString "inGround"
    val _		= Inf.inGround : unit -> kind		(* verify *)
    val lab_inDependent	= Label.fromString "inDependent"
    val _		= Inf.inDependent : path * inf * kind -> kind (*verify*)

    val lab_intersect	= Label.fromString "intersect"
    val _		= Inf.intersect : inf * inf -> inf	(* verify *)

  end
