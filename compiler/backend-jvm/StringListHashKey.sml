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

structure StringListHashKey :> HASH_KEY where type t = string list =
    struct
	type t = string list

	fun hash strings =
	    Word.toInt
	    (List.foldr (fn (s, w) =>
			 Word.xorb (Word.fromInt (StringHashKey.hash s), w))
	     0w0 strings)
    end
