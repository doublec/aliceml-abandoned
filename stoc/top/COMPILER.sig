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
 * Scenarios for compiler usage:
 * 1. Batch: always use empty context, discard result
 * 2. Interactive: use previous resulting context
 * 3. Debugging: context is provided by some outside magic, discard result
 *)

signature COMPILER =
  sig
    structure Switches: SWITCHES
    structure Target: TARGET

    type context

    val empty:   context
    val compile: context * Source.desc * Source.t ->
		 context * Target.t  (* [Error.Error] *)
  end
