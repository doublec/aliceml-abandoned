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
	structure Switches = MakeSwitches(val logOut = TextIO.stdOut)

	structure ComPlusTarget = MakeComPlusTarget(Signature)

	structure FrontendSML =
	    MakeFrontendSML(structure Composer = Composer
			    structure Switches = Switches)

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
		     structure BackendSpecific  = BackendComPlus)
    end
