signature INF =
  sig

  (* Types *)

    type lab   = Label.t
    type name  = Name.t
    type stamp = Stamp.t
    type path  = Path.t
    type typ   = Type.t
    type tkind = Type.kind
    type fix   = Fixity.t

    datatype val_sort = VALUE | CONSTRUCTOR		(* [w] *)
    datatype typ_sort = datatype Type.sort		(* [w] *)

    type kind						(* [kappa,k] *)
    type con = kind * path				(* [chi,c]   *)
    type sign						(* [sigma,s] *)
    type item
    type inf						(* [jota,j] *)
    type t = inf    

  (* Realisations *)

    type val_rea = path PathMap.t
    type typ_rea = typ  PathMap.t
    type mod_rea = path PathMap.t
    type inf_rea = inf  PathMap.t

    type rea	 = { val_rea : val_rea
		   , typ_rea : typ_rea
		   , mod_rea : mod_rea
		   , inf_rea : inf_rea
		   }

  (* Injections *)

    val inTop :		unit		 -> inf
    val inCon :		con		 -> inf
    val inSig :		sign		 -> inf
    val inArrow :	path * inf * inf -> inf
    val inLambda :	path * inf * inf -> inf
    val inApply :	inf * path * inf -> inf

  (* Inquiries *)

    val isTop :		inf -> bool
    val isCon :		inf -> bool
    val isSig :		inf -> bool
    val isArrow :	inf -> bool
    val isLambda :	inf -> bool
    val isApply :	inf -> bool

  (* Projections *)

    exception Interface

    val asCon :		inf -> con			(* Interface *)
    val asSig :		inf -> sign			(* Interface *)
    val asArrow :	inf -> path * inf * inf		(* Interface *)
    val asLambda :	inf -> path * inf * inf		(* Interface *)
    val asApply :	inf -> inf * path * inf		(* Interface *)

  (* Cloning etc. *)

    val clone :		inf -> inf
    val instance :	inf -> inf
    val singleton :	inf -> inf
    val realise :	rea  * inf -> unit
    val strengthen :	path * inf -> unit
    val strengthenSig :	path * sign -> unit

  (* Kinds *)

    exception Kind

    val inGround :	unit -> kind
    val inDependent :	path * inf * kind -> kind

    val isGround :	kind -> bool
    val isDependent :	kind -> bool
    val asDependent :	kind -> path * inf * kind	(* Kind *)

    val kind :		inf -> kind

  (* Signature construction *)

    val empty :		unit -> sign

    val newVal :	sign * lab -> path
    val newTyp :	sign * lab -> path
    val newMod :	sign * lab -> path
    val newInf :	sign * lab -> path
    val newFix :	sign * lab -> path

    val extendVal :	sign * path *  typ  * val_sort * path option -> unit
    val extendTyp :	sign * path * tkind * typ_sort * typ  option -> unit
    val extendMod :	sign * path *  inf  * path option -> unit
    val extendInf :	sign * path *  kind * inf  option -> unit
    val extendFix :	sign * path * fix -> unit

  (* Signature inspection *)

    val items :		sign -> item list

    exception Item

    val isValItem :	item -> bool
    val isTypItem :	item -> bool
    val isModItem :	item -> bool
    val isInfItem :	item -> bool
    val isFixItem :	item -> bool

    val asValItem :	item -> lab * typ * val_sort * path option  (* Item *)
    val asTypItem :	item -> lab * tkind * typ_sort * typ option (* Item *)
    val asModItem :	item -> lab * inf * path option		(* Item *)
    val asInfItem :	item -> lab * kind * inf option		(* Item *)
    val asFixItem :	item -> lab * fix			(* Item *)

  (* Signature lookup *)

    exception Lookup

    val lookupVal :	sign * lab -> typ		(* Lookup *)
    val lookupTyp :	sign * lab -> typ		(* Lookup *)
    val lookupMod :	sign * lab -> inf		(* Lookup *)
    val lookupInf :	sign * lab -> inf		(* Lookup *)
    val lookupFix :	sign * lab -> fix		(* Lookup *)

    val lookupVal' :	sign * lab * int -> typ		(* Lookup *)
    val lookupTyp' :	sign * lab * int -> typ		(* Lookup *)
    val lookupMod' :	sign * lab * int -> inf		(* Lookup *)
    val lookupInf' :	sign * lab * int -> inf		(* Lookup *)
    val lookupFix' :	sign * lab * int -> fix		(* Lookup *)

    val lookupValSort :	sign * lab -> val_sort		(* Lookup *)
    val lookupTypSort :	sign * lab -> typ_sort		(* Lookup *)
    val lookupValPath :	sign * lab -> path		(* Lookup *)
    val lookupModPath :	sign * lab -> path		(* Lookup *)

  (* Closure check *)

    exception Unclosed of lab * int * typ

    val close :		sign -> unit			(* Unclosed *)

  (* Matching and intersection *)

    datatype mismatch =
	  MissingVal  of lab
	| MissingTyp  of lab
	| MissingMod  of lab
	| MissingInf  of lab
	| MissingFix  of lab
	| ManifestVal of lab
	| ManifestTyp of lab
	| ManifestMod of lab
	| ManifestInf of lab
	| MismatchVal of lab * typ * typ
	| MismatchTyp of lab * tkind * tkind
	| MismatchMod of lab * mismatch
	| MismatchInf of lab * mismatch
	| MismatchFix of lab * fix * fix
	| MismatchValSort of lab * val_sort * val_sort
	| MismatchTypSort of lab * typ_sort * typ_sort
	| MismatchDom     of mismatch
	| MismatchRan     of mismatch
	| Incompatible    of inf * inf
	| IncompatibleArg of path * path

    exception Mismatch of mismatch

    val match :		inf * inf -> rea		(* Mismatch *)
    val intersect :	inf * inf -> inf		(* Mismatch *)
    val equaliseKind :	kind * kind -> unit		(* Mismatch *)

  end
