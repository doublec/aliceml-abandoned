(*
 * Authors:
 *   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
 *
 * Copyright:
 *   Thorsten Brunklaus, 2000
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

signature SEARCH =
    sig
	type 'a pruner = 'a * 'a -> unit

	val searchOne : (unit -> 'a) -> 'a option
	val searchAll : (unit -> 'a) -> 'a list
	val searchBest : (unit -> 'a) * 'a pruner -> 'a option
    end
