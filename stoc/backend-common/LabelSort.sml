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

functor LabelSort(type t val get: t -> string) :> LABELSORT
    where type t = t =
    struct
	type t = t

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

	fun labelLess (x1, x2) =
	    let
		val s1 = get x1
		val s2 = get x2
	    in
		case Int.fromString s1 of
		    SOME i1 =>
			(case Int.fromString s2 of
			     SOME i2 => i1 < i2
			   | NONE => true)
		  | NONE => String.< (s1, s2)
	    end

	fun merge (xs as x::xr, ys as y::yr) =
	    if labelLess (x, y) then x::merge (xr, ys)
	    else y::merge (xs, yr)
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
	    if get x = Int.toString i then isTuple (xr, i + 1)
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
