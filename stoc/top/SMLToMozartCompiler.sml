(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
 *   Andreas Rossberg, 1999-2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

local
    structure Switches = MakeSwitches()

    structure MozartTarget = MakeMozartTarget(Signature)

    structure FrontendSML = MakeFrontendSML(Composer)

    structure FrontendCommon =
	MakeFrontendCommon(structure Composer = Composer
			   structure Switches = Switches)

    structure BackendMozart = MakeBackendMozart(MozartTarget)
in
    structure SMLToMozartCompiler =
	MakeCompiler(structure Switches         = Switches
		     structure Target           = MozartTarget
		     structure FrontendSpecific = FrontendSML
		     structure FrontendCommon   = FrontendCommon
		     structure BackendCommon    = BackendCommon
		     structure BackendSpecific  = BackendMozart

		     structure FrontendSpecificInitialContext =
			       FrontendSMLInitialContext
		     structure FrontendCommonInitialContext =
			       FrontendCommonInitialContext
		     structure BackendCommonInitialContext =
			       InitialEmptyContext
		     structure BackendSpecificInitialContext =
			       InitialEmptyContext)
end
