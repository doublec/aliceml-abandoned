(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Composer: COMPOSER =
    struct
	structure Sig = Signature

	exception Corrupt
	exception Conflict

	structure UrlMap = MakeHashImpMap(FromEqHashKey(Url))

	val signTable: Sig.t UrlMap.t = UrlMap.new ()

	fun sign url = UrlMap.lookup (signTable, url)

	fun enterSign (url, sign) =
	    UrlMap.insertDisjoint (signTable, url, sign)
	    handle UrlMap.Collision _ => raise Conflict

	fun start url = ()
    end
