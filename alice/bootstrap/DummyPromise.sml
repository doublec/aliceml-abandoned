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
	val future : 'a promise -> 'a
    end

structure Promise =
    struct
	type 'a promise = 'a Option.option ref
	type 'a t = 'a promise

	exception Promise

	fun promise () = ref NONE
	fun fulfill (p, x) = p := SOME x
	fun future (ref (SOME x)) = x
	  | future (ref NONE) = raise Promise
    end
