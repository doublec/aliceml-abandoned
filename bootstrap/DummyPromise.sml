(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2003
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(* Dummy replacement for bootstrapping *)

signature PROMISE =
    sig
	type 'a promise = 'a option ref
	type 'a t = 'a promise

	val promise : unit -> 'a promise
	val fulfill : 'a promise * 'a  -> unit
    end

structure Promise =
    struct
	type 'a promise = 'a Option.option ref
	type 'a t = 'a promise

	fun promise () = ref NONE
	fun fulfill (p, x) = p := SOME x
    end
