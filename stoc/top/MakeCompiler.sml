(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(*
 * Mmh, how about parameterization of Phases over error output formatting...?
 * And shouldn't the Composer also be applied here?
 * Problem is, this requires applicative functors with extended paths.
 *)

functor MakeCompiler(
	structure Switches:         SWITCHES
	structure Target:           TARGET
	structure FrontendSpecific: PHASE where type I.t = Source.t
	structure FrontendCommon:   PHASE where I = FrontendSpecific.O
	structure BackendCommon:    PHASE where I = FrontendCommon.O
	structure BackendSpecific:  PHASE where I = BackendCommon.O
					  where type O.t = Target.t
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

    val empty =
	{ feSpec = FrontendSpecific.C.new()
	, feComm = FrontendCommon.C.new()
	, beComm = BackendCommon.C.new()
	, beSpec = BackendSpecific.C.new()
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
