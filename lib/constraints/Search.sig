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
	type order = 'a * 'a -> unit
	    
	val searchOne : (unit -> 'a) -> 'a option
	val searchAll : (unit -> 'a) -> 'a list
	val searchBest : (unit -> 'a) * order -> 'a option
	    
(*	val exploreOne : (unit -> 'a) -> unit
	val exploreAll : (unit -> 'a) -> unit
	val exploreBest : (unit -> 'a) -> unit *)
    end
