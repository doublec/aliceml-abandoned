(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature LABEL_SORT =
    sig
	type 'a t

	datatype arity =
	    Tup of int
	  | Prod

	val sort: 'a t list -> 'a t vector * arity
    end
