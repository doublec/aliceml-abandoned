(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

functor MakeLabelSort(type 'a t val get: 'a t -> Label.t) :> LABEL_SORT
    where type 'a t = 'a t =
    struct
	type 'a t = 'a t

	structure Sort =
	    MakeSort (type 'a t = 'a t
		      fun compare (x1, x2) = Label.compare (get x1, get x2))

	datatype arity =
	    Rec
	  | Tup of int

	fun isTuple (x::xr, i) =
	    if get x = Label.fromInt i then isTuple (xr, i + 1)
	    else NONE
	  | isTuple (nil, i) = SOME (i - 1)

	fun sort xs =
	    let
		val xs' = Sort.sort xs
	    in
		case isTuple (xs', 1) of
		    SOME i => (xs', Tup i)
		  | NONE => (xs', Rec)
	    end
    end
