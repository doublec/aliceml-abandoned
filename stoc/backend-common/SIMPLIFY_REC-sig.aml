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

signature SIMPLIFY_REC =
    sig
	structure I: INTERMEDIATE_GRAMMAR = IntermediateGrammar

	val derec: I.pat * I.exp ->
	    (I.longid * I.longid) list * (I.id list * I.exp) list
    end
