(*
 * Mmh, how about parameterization of Phases over error output formatting...?
 * And shouldn't the Composer also be applied here?
 * Problem is, this requires applicative functors with extended paths.
 *)

functor MakeCompiler(
	structure Switches:         SWITCHES
	structure Target:           TARGET
	structure FrontendSpecific: PHASE where I = Source
	structure FrontendCommon:   PHASE where I = FrontendSpecific.O
	structure BackendCommon:    PHASE where I = FrontendCommon.O
	structure BackendSpecific:  PHASE where I = BackendCommon.O
					  where O = Target
	structure FrontendSpecificInitialContext: INITIAL_CONTEXT
					  where type t = FrontendSpecific.C.t
	structure FrontendCommonInitialContext:   INITIAL_CONTEXT
					  where type t = FrontendCommon.C.t
	structure BackendCommonInitialContext:    INITIAL_CONTEXT
					  where type t = BackendCommon.C.t
	structure BackendSpecificInitialContext:  INITIAL_CONTEXT
					  where type t = BackendSpecific.C.t
       ) :> COMPILER where Target = Target
  =
  struct
    structure Switches = Switches
    structure Target = Target

    type context =
	{ feSpec : FrontendSpecific.C.t
	, feComm : FrontendCommon.C.t
	, beComm : BackendCommon.C.t
	, beSpec : BackendSpecific.C.t
	}

    val initial =
	{ feSpec = FrontendSpecificInitialContext.initial()
	, feComm = FrontendCommonInitialContext.initial()
	, beComm = BackendCommonInitialContext.initial()
	, beSpec = BackendSpecificInitialContext.initial()
	}

    fun clone {feSpec,feComm,beComm,beSpec} =
	{ feSpec = FrontendSpecific.C.clone feSpec
	, feComm = FrontendCommon.C.clone feComm
	, beComm = BackendCommon.C.clone beComm
	, beSpec = BackendSpecific.C.clone beSpec
	}

    fun compile(C, desc, source) =
	let
	    val C' = clone C
	    val rep = FrontendSpecific.translate (#feSpec C') (desc, source)
	    val rep = FrontendCommon.translate (#feComm C') (desc, rep)
	    val rep = BackendCommon.translate (#beComm C') (desc, rep)
	    val target = BackendSpecific.translate (#beSpec C') (desc, rep)
	in
	    (C', target)
	end
  end
