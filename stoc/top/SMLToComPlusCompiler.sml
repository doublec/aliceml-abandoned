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

structure SMLToComPlusCompiler =
    let
	structure Switches = MakeSwitches()

	structure ComPlusTarget = MakeComPlusTarget(Signature)

	structure FrontendSML = MakeFrontendSML(Composer)

	structure FrontendCommon =
	    MakeFrontendCommon(structure Composer = Composer
			       structure Switches = Switches)

	structure BackendComPlus = MakeBackendComPlus(ComPlusTarget)
    in
	MakeCompiler(structure Switches         = Switches
		     structure Target           = ComPlusTarget
		     structure FrontendSpecific = FrontendSML
		     structure FrontendCommon   = FrontendCommon
		     structure BackendCommon    = BackendCommon
		     structure BackendSpecific  = BackendComPlus

		     structure FrontendSpecificInitialContext =
			       FrontendSMLInitialContext
		     structure FrontendCommonInitialContext =
			       FrontendCommonInitialContext
		     structure BackendCommonInitialContext =
			       InitialEmptyContext
		     structure BackendSpecificInitialContext =
			       InitialEmptyContext)
    end
