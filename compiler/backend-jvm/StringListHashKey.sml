(*
 * Author:
 *   Andy Walter <anwalt@ps.uni-sb.de>
 *
 * Copyright:
 *   Andy Walter, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure StringListHashKey =
  struct
      open Main
      datatype lab = datatype IntermediateGrammar.lab

    type t = string list

    open Word
    infix << >> andb xorb

    fun hash strings =  (* hashpjw [Aho/Sethi/Ullman "Compilers"] *)
	let
	    val n = List.length strings

	    fun iter(string'::strings,h) =
		let
		    val c  = fromInt(StringHashKey.hash string')
		    val h' = (h << 0w4) + c
		    val g  = h' andb 0wxf00000
		in
		    iter(strings, h' xorb g xorb (g >> 0w16))
		end
	  | iter (nil, h) = h
	in
	    toInt(iter(strings,0w0))
	end
  end
