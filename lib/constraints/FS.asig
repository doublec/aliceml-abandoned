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

import structure FD from "x-alice:/lib/constraints/FD.ozf"

local
    open FD
in
    signature FS_COMPONENT =
	sig
	    signature FS =
		sig
		    type fs

		    (* Allocation Function *)
		    val fs : domain * domain -> fs
		    val fsvector : domain * domain * int -> fs vector
		    val decl : unit -> fs

		    (* Integer FS *)
		    structure Int :
			sig
			    val min : fs * fd -> unit
			    val max : fs * fd -> unit
			    val convex : fs -> unit
			    val match : fs * fd vector -> unit
			    val minN : fs * fd vector -> unit
			    val maxN : fs * fd vector -> unit
			end

		    (* Standard Propagators *)
		    val compl : fs * fs -> unit
		    val complIn : fs * fs * fs -> unit
		    val include' : fd * fs -> unit
		    val exclude : fd * fs -> unit
		    val card : fs * fd -> unit
		    val cardRange : int * int * fs -> unit
		    val isIn : int * fs -> bool
		    val empty : fs -> bool

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

		    (* FS Values *)
		    structure Value :
			sig
			    val empty : unit -> fs
			    val universal : unit -> fs
			    val singl : int -> fs
			    val make : domain -> fs
			    val is : fs -> bool
			end
		    
		    (* Reified Propagators *)
		    structure Reified :
			sig
			    val isIn : int * fs * bin -> unit
			    val areIn : int list * fs * bin list -> unit
			    val include' : fd * fs * bin -> unit
			    val equal : fs * fs * bin -> unit
			    val partition : fs list * int list * fs * bin list -> unit
			end

		    (* Reflection *)
		    structure Reflect :
			sig
			    val card : fs -> domain
			    val lowerBound : fs -> domain
			    val unknown : fs -> domain
			    val upperBound : fs -> domain

			    structure CardOf :
				sig
				    val lowerBound : fs -> int
				    val unknown : fs -> int
				    val upperBound : fs -> int
				end
			end
		end

	    structure FS : FS
	end
end
