signature INF =
  sig

  (* Types *)

    type lab   = Lab.t
    type name  = Name.t
    type stamp = Stamp.t
    type path  = Path.t
    type typ   = Type.t
    type tkind = Type.kind

    datatype val_sort = VALUE | CONSTRUCTOR	(* [w] *)
    datatype typ_sort = datatype Type.sort	(* [w] *)

    type kind					(* [kappa,k] *)
    type con  = kind * path			(* [chi,c]   *)
    type sign					(* [sigma,s] *)
    type inf					(* [jota,j] *)
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

    val inAny :		unit		 -> inf
    val inCon :		con		 -> inf
    val inSig :		sign		 -> inf
    val inArrow :	path * inf * inf -> inf
    val inLambda :	path * inf * inf -> inf
    val inApp :		inf * path * inf -> inf

  (* Inquiries *)

    val isAny :		inf -> bool
    val isCon :		inf -> bool
    val isSig :		inf -> bool
    val isArrow :	inf -> bool
    val isLambda :	inf -> bool
    val isApp :		inf -> bool

  (* Projections *)

    exception Interface

    val asCon :		inf -> con			(* Interface *)
    val asSig :		inf -> sign			(* Interface *)
    val asArrow :	inf -> path * inf * inf		(* Interface *)
    val asLambda :	inf -> path * inf * inf		(* Interface *)
    val asApp :		inf -> inf * path * inf		(* Interface *)

  (* Cloning etc. *)

    val clone :		inf -> inf
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

    val extendVal :	sign * path *  typ  * val_sort * path option -> path
    val extendTyp :	sign * path * tkind * typ_sort * typ  option -> path
    val extendMod :	sign * path *  inf  * path option -> path
    val extendInf :	sign * path *  kind * inf  option -> path

  (* Signature lookup *)

    val lookupVal :	sign * lab -> typ
    val lookupTyp :	sign * lab -> typ
    val lookupMod :	sign * lab -> inf
    val lookupInf :	sign * lab -> inf

    val lookupVal' :	sign * lab * int -> typ
    val lookupTyp' :	sign * lab * int -> typ
    val lookupMod' :	sign * lab * int -> inf
    val lookupInf' :	sign * lab * int -> inf

  (* Matching and intersection *)

    datatype mismatch =
	  MissingVal  of lab
	| MissingTyp  of lab
	| MissingMod  of lab
	| MissingInf  of lab
	| ManifestVal of lab
	| ManifestTyp of lab
	| ManifestMod of lab
	| ManifestInf of lab
	| MismatchVal of lab * typ * typ
	| MismatchTyp of lab * tkind * tkind
	| MismatchMod of lab * mismatch
	| MismatchInf of lab * mismatch
	| Incompatible    of inf * inf
	| IncompatibleArg of path * path

    exception Mismatch of mismatch

    val match :		inf * inf -> rea		(* Mismatch *)
    val intersect :	inf * inf -> inf		(* Mismatch *)

  end
