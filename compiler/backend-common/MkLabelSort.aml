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

	datatype arity =
	    Rec
	  | Tup of int

	fun split nil = (nil, nil)
	  | split (xs as [_]) = (xs, nil)
	  | split (x1::x2::xr) =
	    let
		val (xr1, xr2) = split xr
	    in
		(x1::xr1, x2::xr2)
	    end

	fun labelLess (x1, x2) = Label.compare (get x1, get x2)

	fun merge (xs as x::xr, ys as y::yr) =
	    (case labelLess (x, y) of
		 LESS => x::merge (xr, ys)
	       | EQUAL => raise Crash.Crash "MakeLabelSort.merge"
	       | GREATER => y::merge (xs, yr))
	  | merge (nil, ys) = ys
	  | merge (xs, nil) = xs

	fun sort' nil = nil
	  | sort' (xs as [_]) = xs
	  | sort' xs =
	    let
		val (ys, zs) = split xs
	    in
		merge (sort' ys, sort' zs)
	    end

	fun isTuple (x::xr, i) =
	    if get x = Label.fromInt i then isTuple (xr, i + 1)
	    else NONE
	  | isTuple (nil, i) = SOME (i - 1)

	fun sort xs =
	    let
		val xs' = sort' xs
	    in
		case isTuple (xs', 1) of
		    SOME i => (xs', Tup i)
		  | NONE => (xs', Rec)
	    end
    end
