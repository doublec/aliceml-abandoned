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

structure LitHashKey :> HASH_KEY where type t = FlatGrammar.lit =
    struct
	datatype lit = datatype FlatGrammar.lit
	type t = lit

	open LargeWord

	fun hash (WordLit w) = toInt (andb (w, 0wxf00000))
	  | hash (IntLit i) = toInt (andb (fromLargeInt i, 0wxf00000))
	  | hash (CharLit c) = Char.ord c
	  | hash (StringLit s) = StringHashKey.hash s
	  | hash (RealLit s) = StringHashKey.hash s
    end
