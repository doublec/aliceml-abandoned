(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 1999-2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure SMLToMozartCompiler =
    let
	structure Switches = MakeSwitches(val logOut = TextIO.stdOut)

	structure MozartTarget =
	    MakeMozartTarget(structure Switches = Switches
			     structure Sig = Signature)

	structure FrontendSML = MakeFrontendSML(Composer)

	structure FrontendCommon =
	    MakeFrontendCommon(structure Composer = Composer
			       structure Switches = Switches)

	structure BackendMozart = MakeBackendMozart(MozartTarget)
    in
	MakeCompiler(structure Switches         = Switches
		     structure Target           = MozartTarget
		     structure FrontendSpecific = FrontendSML
		     structure FrontendCommon   = FrontendCommon
		     structure BackendCommon    = BackendCommon
		     structure BackendSpecific  = BackendMozart)
    end
