(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure SMLToStockwerkCompiler =
    let
	structure Switches = MakeSwitches(val logOut = TextIO.stdOut)

	structure PickleTarget =
	    MakePickleTarget(structure Sig = Signature)

	structure FrontendSML =
	    MakeFrontendSML(structure Composer = Composer
			    structure Switches = Switches)

	structure FrontendCommon =
	    MakeFrontendCommon(structure Composer = Composer
			       structure Switches = Switches)

	structure BackendCommon = MakeBackendCommon(Switches)

	structure BackendStockwerk =
	    MakeBackendStockwerk(structure PickleTarget = PickleTarget)
    in
	MakeCompiler(structure Switches         = Switches
		     structure Target           = PickleTarget
		     structure FrontendSpecific = FrontendSML
		     structure FrontendCommon   = FrontendCommon
		     structure BackendCommon    = BackendCommon
		     structure BackendSpecific  = BackendStockwerk)
    end
