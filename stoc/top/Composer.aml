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

structure Composer: COMPOSER' =
    struct
	structure Sig = Signature

	exception Corrupt

	val acquire: (Url.t -> Sig.t) ref =
	    ref (fn _ => raise Crash.Crash "Composer.acquire")

	fun setAcquisitionMethod f = acquire := f

	structure UrlMap = MakeHashImpMap(FromEqHashKey(Url))

	val signTable: Sig.t UrlMap.t = UrlMap.new ()

	fun sign url =
	    case UrlMap.lookup (signTable, url) of
		SOME sign => sign
	      | NONE =>
		    let
			val sign = !acquire url
		    in
			UrlMap.insertDisjoint (signTable, url, sign);
			sign
		    end

	fun start url = ()
    end
