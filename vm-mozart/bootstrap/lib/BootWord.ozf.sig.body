(* -*- sml -*- *)

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

signature WORD_COMPONENT =
    sig
	structure BootWord:
	    sig
		val fromInt' : int * int -> word
		val toInt : word -> int
		val toLargeInt : word -> LargeInt.int
		val toIntX : word -> int
		val op+ : word * word -> word
		val op- : word * word -> word
		val op* : word * word -> word
		val op mod : word * word -> word
		val orb : word * word -> word
		val xorb : word * word -> word
		val andb : word * word -> word
		val notb : word * word -> word
		val op<< : word * word -> word
		val op>> : word * word -> word
		val op~>> : word * word -> word
		val toString : word -> string
	    end
    end
