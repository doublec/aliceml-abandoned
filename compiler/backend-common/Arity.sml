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

structure Arity :> ARITY =
    struct
	datatype t =
	    Unary
	  | Tuple of int
	  | Product of Label.t vector
	    (* sorted, all labels distinct, no tuple *)
    end
