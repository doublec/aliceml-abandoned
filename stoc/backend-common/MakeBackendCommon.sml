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
structure BackendCommon: PHASE =
    ComposePhases(structure Phase1 = FlatteningPhase
		  structure Phase2 = ValuePropagationPhase
		  structure Context = EmptyContext
		  fun context1 () = ()
		  fun context2 () = ())
*)

structure BackendCommon: PHASE = FlatteningPhase
