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

structure LabelSort :> sig val sort: string list -> string list end =
    struct
	fun split nil = (nil, nil)
	  | split (xs as [_]) = (xs, nil)
	  | split (x1::x2::xr) =
	    let
		val (xr1, xr2) = split xr
	    in
		(x1::xr1, x2::xr2)
	    end

	fun labelLess (s1, s2) =
	    case Int.fromString s1 of
		SOME i1 =>
		    (case Int.fromString s2 of
			 SOME i2 => i1 < i2
		       | NONE => true)
	      | NONE => String.< (s1, s2)

	fun merge (xs as x::xr, ys as y::yr) =
	    if labelLess (x, y) then x::merge (xr, ys)
	    else y::merge (xs, yr)
	  | merge (nil, ys) = ys
	  | merge (xs, nil) = xs

	fun sort nil = nil
	  | sort (ss as [_]) = ss
	  | sort ss =
	    let
		val (xs, ys) = split ss
	    in
		merge (sort xs, sort ys)
	    end
    end
