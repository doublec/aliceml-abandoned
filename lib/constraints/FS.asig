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

signature FS_COMPONENT =
    sig
	signature FS =
	    sig
		type fs

		datatype domain_element =
		    SINGLE of int
		  | RANGE  of int * int

		type domain = domain_element vector

		(* Allocation Functions *)
		val fs : domain -> fs
		val compl : fs -> fs
		val complIn : fs * fs -> fs
		val include' : fs -> fs (* to be determined *)
		val exclude : fs -> fs (* to be determined *)

		val empty : fs -> bool
		val card : fs -> fs
		val cardRange : int * int * fs -> unit
		val isIn : int * fs -> bool

		(* Integer Sets *)
		structure Int :
		    sig
			val
		    end

		(* Standard Propagators *)
		val diff : fs * fs * fs -> unit
		val intersect : fs * fs * fs -> unit
		val intersectN : fs vector * fs -> unit
		val union : fs * fs * fs -> unit
		val unionN : fs vector * fs -> unit
		val subset : fs * fs -> unit
		val disjoint : fs * fs -> unit
		val disjointN : fs vector -> unit
		val distinct : fs * fs -> unit
		val distictN : fs vector -> unit
		val partition : fs vector * fs -> unit
	    end

	structure FS : FS
    end
