(*
 * Scenarios for compiler usage:
 * 1. Batch: always use initial context, discard result
 * 2. Interactive: use previous resulting context
 * 3. Debugging: context is provided by some outside magic, discard result
 *)

signature COMPILER =
  sig
    structure Switches: SWITCHES
    structure Target: TARGET

    type context

    val initial: context
    val compile: context * Source.desc * Source.t ->
		 context * Target.t  (* [Error.Error] *)
  end
