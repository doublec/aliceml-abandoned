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

structure Compiler =
    MakeCompiler(structure Target           = MozartTarget
		 structure FrontendSpecific = MakeFrontendSML(Composer)
		 structure FrontendCommon   = MakeFrontendCommon(Composer)
		 structure BackendCommon    = BackendCommon
		 structure BackendSpecific  = MozartGenerationPhase
		 structure FrontendSpecificInitialContext =
		     struct
			 type t = BindEnv.t
			 fun initial () = BindEnv.clone BindEnv0.E0
		     end
		 structure FrontendCommonInitialContext =
		     struct
			 type t = Env.t
			 fun initial () = Env.clone Env0.E0
		     end
		 structure BackendCommonInitialContext =
		     struct
			 type t = unit
			 fun initial () = ()
		     end
		 structure BackendSpecificInitialContext =
		     struct
			 type t = unit
			 fun initial () = ()
		     end)
