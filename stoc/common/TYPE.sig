signature TYPE =
  sig

  (* Types *)

    datatype sort = OPEN | CLOSED
    datatype kind = STAR | ARROW of kind * kind		(* [kappa,k] *)

    type lab  = Label.t					(* [lab,l] *)
    type path = Path.t					(* [pi,p] *)
    type con  = kind * sort * path			(* [chi,c] *)

    type row						(* [rho,r] *)
    type var						(* [alpha,a] *)
    type typ						(* [tau,t] *)
    type t = typ

    type path_rea = path PathMap.t
    type typ_rea  = typ PathMap.t


  (* Injections *)

    val unknown :	kind      -> typ
    val inArrow :	typ * typ -> typ
    val inTuple :	typ list  -> typ
    val inProd :	row       -> typ
    val inSum :		row       -> typ
    val inVar :		var       -> typ
    val inCon :		con       -> typ
    val inAll :		var * typ -> typ
    val inExist :	var * typ -> typ
    val inLambda :	var * typ -> typ
    val inApply :	typ * typ -> typ
    val inMu :		typ       -> typ
    val inAbbrev :	typ * typ -> typ

    val var :		kind -> var

  (* Inquiries *)

    val isUnknown :	typ -> bool
    val isArrow :	typ -> bool
    val isTuple :	typ -> bool
    val isProd :	typ -> bool
    val isSum :		typ -> bool
    val isVar :		typ -> bool
    val isCon :		typ -> bool
    val isAll :		typ -> bool
    val isExist :	typ -> bool
    val isLambda :	typ -> bool
    val isApply :	typ -> bool
    val isMu :		typ -> bool
    val isAbbrev :	typ -> bool

  (* Projections *)

    exception Type

    val asArrow :	typ -> typ * typ		(* Type *)
    val asTuple :	typ -> typ list			(* Type *)
    val asProd :	typ -> row			(* Type *)
    val asSum :		typ -> row			(* Type *)
    val asVar :		typ -> var			(* Type *)
    val asCon :		typ -> con			(* Type *)
    val asAll :		typ -> var * typ		(* Type *)
    val asExist :	typ -> var * typ		(* Type *)
    val asLambda :	typ -> var * typ		(* Type *)
    val asApply :	typ -> typ * typ		(* Type *)
    val asMu :		typ -> typ			(* Type *)
    val asAbbrev : 	typ -> typ * typ		(* Type *)

  (* Complex extractions *)

    val kind :		typ -> kind
    val kindVar :	var -> kind

    val path :		typ -> path			(* Type *)
    val pathCon :	con -> path

    val paths :		typ -> PathSet.t

  (* Operations on rows *)

    exception Row

    val unknownRow :	unit -> row
    val emptyRow :	unit -> row
    val extendRow :	lab * typ list * row -> row	(* Row *)

    val openRowType :	typ -> unit			(* Row *)

    val isEmptyRow :	row -> bool
    val isUnknownRow :	row -> bool
    val headRow :	row -> lab * typ list		(* Row *)
    val tailRow :	row -> row			(* Row *)

  (* Copying and instantiation *)

    val instance :	typ -> typ
    val skolem :	typ -> typ
    val clone :		typ -> typ

    type clone_state
    val cloneStart :	unit -> clone_state
    val cloneCont :	clone_state -> typ -> typ
    val cloneFinish :	clone_state -> unit

    val realise :	typ_rea  * typ -> unit
    val realisePath :	path_rea * typ -> unit

  (* Unification and closure *)

    exception Unify of typ * typ
    exception UnifyList of int * typ * typ
    exception Intersect

    val fill :		typ * typ -> unit
    val unify :		typ * typ -> unit		(* Unify *)
    val unifyList :	typ list  -> unit		(* UnifyList *)
    val intersect :	typ * typ -> unit
    val close :		typ -> typ
    val isClosed :	typ -> bool

  (* Comparison *)

    val equals :	typ * typ -> bool
    val matches :	typ * typ -> bool

  (* Level management *)

    exception Lift of var

    val lift :		typ  -> unit			(* Lift *)
    val enterLevel :	unit -> unit
    val exitLevel :	unit -> unit
    val resetLevel :	unit -> unit

  end
