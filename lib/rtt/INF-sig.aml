signature INF =
  sig

  (* Types *)

    type path = Path.t

    type inf
    type t = inf    

    type kind					(* [kappa,k] *)
    type con  = kind * path			(* [chi,c]   *)
    type sign = (inf,kind) Sign.t		(* [sigma,s] *)

    type rea  = Sign.rea
    type rea' = (inf,kind) Sign.rea'

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

  (* Kinds *)

    exception Kind

    val inGround :	unit -> kind
    val inDependent :	path * inf * kind -> kind

    val isGround :	kind -> bool
    val isDependent :	kind -> bool
    val asDependent :	kind -> path * inf * kind	(* Kind *)

    val kind :		inf -> kind

  end
