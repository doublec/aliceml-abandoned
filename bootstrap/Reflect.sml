(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2001-2003
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(* Dummy replacement for bootstrapping *)

structure Reflect =
    struct
	type value  = unit
	type module = unit

	fun realToVector r =
	    let
		val vec = Unsafe.blastWrite r
		val mkIndex =
		    case Word8Vector.sub (vec, 0) of
			0wx33 => (fn i => 103 - i)
		      | 0wx00 => (fn i => i + 96)
		      | _ => raise Match
	    in
		Word8Vector.tabulate
		    (8, fn i => Word8Vector.sub (vec, mkIndex i))
	    end
    end
