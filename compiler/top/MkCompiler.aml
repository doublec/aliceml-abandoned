(*
 * Mmh, how about parameterization of Phases over error output formatting...?
 * And shouldn't the Composer also be applied here?
 * Problem is, this requires applicative functors with extended paths.
 *)

functor MakeCompiler(
	structure Source:   SOURCE
	structure Target:   TARGET
	structure FrontendSpecific: PHASE where I = Source
	structure FrontendCommon:   PHASE where I = FrontendSpecific.O
	structure BackendCommon:    PHASE where I = FrontendCommon.O
	structure BackendSpecific:  PHASE where I = BackendCommon.O
					  where O = Target
       ) :> COMPILER where Source = Source
		     where Target = Target
  =
  struct
    type context =
	{ feSpec: FrontendSpecific.C.t
	, feComm: FrontendCommon.C.t
	, beComm: BackendCommon.C.t
	, beSpec: BackendSpecific.C.t
	}

(*UNFINISHED: Do we need this here? Somehow compromises our modularization.
    val initial =
	{ feSpec = FrontendSpecific.C.initial()
	, feComm = FrontendCommon.C.initial()
	, beComm = BackendCommon.C.initial()
	, beSpec = BackendSpecific.C.initial()
	}
*)

    fun clone {feSpec,feComm,beComm,beSpec} =
	{ feSpec = FrontendSpecific.C.clone feSpec
	, feComm = FrontendCommon.C.clone feComm
	, beComm = BackendCommon.C.clone beComm
	, beSpec = BackendSpecific.C.clone beSpec
	}

    fun compile(C, source) =
	let
	    val C' = clone C
	    val f  = FrontendSpecific.translate(#feSpec C')
		   o FrontendCommon.translate(#feComm C')
		   o BackendCommon.translate(#beComm C')
		   o BackendSpecific.translate(#beSpec C')
	in
	    (C', f source)
	end
  end
