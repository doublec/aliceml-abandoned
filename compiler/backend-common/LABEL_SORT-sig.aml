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

signature LABEL_SORT =
    sig
	type 'a t

	datatype arity =
	    Rec
	  | Tup of int

	val sort: 'a t list -> 'a t list * arity
    end
