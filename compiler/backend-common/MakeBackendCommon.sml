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

(*--**
local
    structure FlatteningAndValuePropagationPhase =
	ComposePhases(structure Phase1 = FlatteningPhase
		      structure Phase2 = ValuePropagationPhase
		      structure Context = EmptyContext
		      fun context1 () = ()
		      fun context2 () = ())
in
    structure BackendCommon =
	ComposePhases(structure Phase1 = FlatteningAndValuePropagationPhase
		      structure Phase2 = LivenessAnalysisPhase
		      structure Context = EmptyContext
		      fun context1 () = ()
		      fun context2 () = ())
end
*)

structure BackendCommon: PHASE = FlatteningPhase
