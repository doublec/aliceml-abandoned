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

structure BackendCommon = FlatteningPhase

(*
structure BackendCommon =
    ComposePhases(structure Phase1 = BackendCommon
		  structure Phase2 = ValuePropagationPhase
		  structure Context = EmptyContext
		  fun context1 () = ()
		  fun context2 () = ())
*)

structure BackendCommon =
    ComposePhases(structure Phase1 = BackendCommon
		  structure Phase2 = LivenessAnalysisPhase
		  structure Context = EmptyContext
		  fun context1 () = ()
		  fun context2 () = ())
