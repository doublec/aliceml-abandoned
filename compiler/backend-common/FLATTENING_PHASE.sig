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

signature FLATTENING_PHASE =
    sig
	structure C: CONTEXT = EmptyContext
	structure I: INTERMEDIATE_GRAMMAR = IntermediateGrammar
	structure O: FLAT_GRAMMAR = FlatGrammar

	val translate: C.t -> Source.desc * I.t -> O.t
    end
