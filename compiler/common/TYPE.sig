signature TYPE =
  sig

  (* Types *)

    datatype con_sort = OPEN | CLOSED

    datatype kind = STAR | ARROW of kind * kind		(* [kappa,k] *)

    type lab  = Lab.t					(* [lab,l] *)
    type path = Path.t					(* [pi,p] *)
    type con  = kind * con_sort * path			(* [chi,c] *)

    type row						(* [rho,r] *)
    type alpha						(* [alpha,a] *)
    type typ						(* [tau,t] *)
    type t = typ

    type rea     = path PathMap.t
    type typ_rea = typ PathMap.t


  (* Injections *)

    val unknown :	kind        -> typ
    val inArrow :	typ * typ   -> typ
    val inTuple :	typ list    -> typ
    val inRow :		row         -> typ
    val inSum :		row         -> typ
    val inVar :		alpha       -> typ
    val inCon :		con         -> typ
    val inAll :		alpha * typ -> typ
    val inExist :	alpha * typ -> typ
    val inLambda :	alpha * typ -> typ
    val inApp :		typ * typ   -> typ
    val inRec :		typ         -> typ

    val var :		kind -> alpha

  (* Inquiries *)

    val isUnknown :	typ -> bool
    val isArrow :	typ -> bool
    val isTuple :	typ -> bool
    val isRow :		typ -> bool
    val isSum :		typ -> bool
    val isVar :		typ -> bool
    val isCon :		typ -> bool
    val isAll :		typ -> bool
    val isExist :	typ -> bool
    val isLambda :	typ -> bool
    val isApp :		typ -> bool

  (* Projections *)

    exception Type

    val asArrow :	typ -> typ * typ	(* Type *)
    val asTuple :	typ -> typ list		(* Type *)
    val asRow :		typ -> row		(* Type *)
    val asSum :		typ -> row		(* Type *)
    val asVar :		typ -> alpha		(* Type *)
    val asCon :		typ -> con		(* Type *)
    val asAll :		typ -> alpha * typ	(* Type *)
    val asExist :	typ -> alpha * typ	(* Type *)
    val asLambda :	typ -> alpha * typ	(* Type *)
    val asApp :		typ -> typ * typ	(* Type *)

  (* Complex extractions *)

    val kind :		typ   -> kind
    val kindVar :	alpha -> kind

    val path :		typ -> path			(* Type *)
    val pathCon :	con -> path

    val paths :		typ -> PathSet.t

  (* Operations on rows *)

    exception Row

    val unknownRow :	unit -> row
    val emptyRow :	unit -> row
    val extendRow :	lab * typ list * row -> row	(* Row *)

  (* Copying and instantiation *)

    val instance :	typ -> typ
    val skolem :	typ -> typ
    val clone :		typ -> typ

    val realise :	rea * typ -> unit
    val realise' :	typ_rea * rea * typ -> unit

  (* Unification and closure *)

    exception Unify of typ * typ
    exception UnifyList of int * typ * typ

    val unify :		typ * typ -> unit		(* Unify *)
    val unifyList :	typ list -> unit		(* UnifyList *)
    val close :		typ -> typ

  (* Level management *)

    val enterLevel :	unit -> unit
    val exitLevel :	unit -> unit

  end
