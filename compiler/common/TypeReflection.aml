structure TypeReflection :> TYPE_REFLECTION =
  struct

  (* Types *)

    val typ_path	= PathReflection.typ_path

    val path_typ	= Path.pervasive "typ"		(*UNFINISHED*)
    val typ_typ		= Type.inCon(Type.STAR, Type.CLOSED, path_typ)
    val path_var	= Path.pervasive "var"		(*UNFINISHED*)
    val typ_var		= Type.inCon(Type.STAR, Type.CLOSED, path_var)
    val path_row	= Path.pervasive "row"		(*UNFINISHED*)
    val typ_row		= Type.inCon(Type.STAR, Type.CLOSED, path_row)

    val lab_open	= Label.fromString "OPEN"
    val lab_closed	= Label.fromString "CLOSED"
    val row_sort	= Type.extendRow(lab_closed, PervasiveType.typ_zero,
			  Type.extendRow(lab_open, PervasiveType.typ_zero,
					 Type.emptyRow()))
    val typ_sort	= Type.inMu(Type.inSum row_sort)
    datatype sort	= datatype Type.sort			(* verify *)
    val _		= OPEN : sort				(* verify *)
    val _		= CLOSED : sort				(* verify *)

    val lab_star	= Label.fromString "STAR"
    val lab_arrow	= Label.fromString "ARROW"
    val typ_kind	= Type.unknown(Type.STAR)
    val typ_kindkind	= Type.inTuple #[typ_kind, typ_kind]
    val row_kind	= Type.extendRow(lab_arrow, typ_kindkind,
			  Type.extendRow(lab_star, PervasiveType.typ_zero,
					 Type.emptyRow()))
    val _		= Type.fill(typ_kind, Type.inMu(Type.inSum row_kind))
    datatype kind	= datatype Type.kind			(* verify *)
    val _		= STAR : kind				(* verify *)
    val _		= ARROW(STAR,STAR) : kind		(* verify *)

    val typ_con		= Type.inTuple #[typ_kind, typ_sort, typ_path]
    type con		= Type.con				(* verify *)
    val _		= (STAR, OPEN, Path.invent()) : con	(* verify *)



  (* The module *)

    val modname_type	= Name.ExId "Type"
    structure Type	= Type					(* verify *)


  (* Type fields *)

    type lab		= Type.lab

    val typname_typ	= Name.ExId "t"
    type typ		= Type.t				(* verify *)
    val typname_var	= Name.ExId "var"
    type var		= Type.var				(* verify *)
    val typname_row	= Name.ExId "row"
    type row		= Type.row				(* verify *)
    val typname_kind	= Name.ExId "kind"
    type kind		= Type.kind				(* verify *)
    val typname_sort	= Name.ExId "sort"
    type sort		= Type.sort				(* verify *)

    (* Labels are defined above *)


  (* Operations *)

    val lab_unknown	= Label.fromString "unknown"
    val _		= Type.unknown : kind -> typ		(* verify *)
    val lab_inArrow	= Label.fromString "inArrow"
    val _		= Type.inArrow : typ * typ -> typ	(* verify *)
    val lab_inTuple	= Label.fromString "inTuple"
    val _		= Type.inTuple : typ vector -> typ	(* verify *)
    val lab_inProd	= Label.fromString "inProd"
    val _		= Type.inProd : row -> typ		(* verify *)
    val lab_inSum	= Label.fromString "inSum"
    val _		= Type.inSum : row -> typ		(* verify *)
    val lab_inVar	= Label.fromString "inVar"
    val _		= Type.inVar : var -> typ		(* verify *)
    val lab_inCon	= Label.fromString "inCon"
    val _		= Type.inCon : con -> typ		(* verify *)
    val lab_inAll	= Label.fromString "inAll"
    val _		= Type.inAll : var * typ -> typ		(* verify *)
    val lab_inExist	= Label.fromString "inExist"
    val _		= Type.inExist : var * typ -> typ	(* verify *)
    val lab_inApply	= Label.fromString "inApply"
    val _		= Type.inApply : typ * typ -> typ	(* verify *)
    val lab_inLambda	= Label.fromString "inLambda"
    val _		= Type.inLambda : var * typ -> typ	(* verify *)
    val lab_inMu	= Label.fromString "inMu"
    val _		= Type.inMu : typ -> typ		(* verify *)

    val lab_var		= Label.fromString "var"
    val _		= Type.var : kind -> var		(* verify *)

    val lab_unknownRow	= Label.fromString "unknownRow"
    val _		= Type.unknownRow : unit -> row		(* verify *)
    val lab_emptyRow	= Label.fromString "emptyRow"
    val _		= Type.emptyRow : unit -> row		(* verify *)
    val lab_extendRow	= Label.fromString "extendRow"
    val _		= Type.extendRow : lab * typ * row -> row (* verify *)

    val lab_fill	= Label.fromString "fill"
    val _		= Type.fill : typ * typ -> unit		(* verify *)

  end
