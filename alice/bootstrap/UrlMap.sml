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

signature URL_MAP =
    sig
	type 'a map
	type 'a t = 'a map

	datatype 'a lookup_result =
	    EXISTING of 'a
	  | NEW of 'a Promise.promise

	val map: (Url.t * 'a) vector -> 'a map

	val lookup: 'a map * Url.t -> 'a option
	val lookupNew: 'a map * Url.t -> 'a lookup_result
    end

structure UrlMap :> URL_MAP =
    struct
	structure UrlMap = MkHashImpMap(Url)

	type 'a map = 'a Promise.promise UrlMap.t
	type 'a t = 'a map

	datatype 'a lookup_result =
	    EXISTING of 'a
	  | NEW of 'a Promise.promise

	fun map initial =
	    let
		val map = UrlMap.map ()
	    in
		Vector.app (fn (url, x) =>
			       UrlMap.insert (map, url, ref (SOME x))) initial;
		map
	    end

	fun lookup (map, url) =
	    case UrlMap.lookup (map, url) of
		SOME x => SOME (Option.valOf (!x))
	      | NONE => NONE

	fun lookupNew (map, url) =
	    case UrlMap.lookup (map, url) of
		SOME x => EXISTING (Option.valOf (!x))
	      | NONE =>
		    let
			val p = Promise.promise ()
		    in
			UrlMap.insertDisjoint (map, url, p);
			NEW p
		    end
    end
