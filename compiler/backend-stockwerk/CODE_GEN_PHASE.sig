(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature CODE_GEN_PHASE =
    sig
	structure C: CONTEXT = EmptyContext
	structure I: FLAT_GRAMMAR = FlatGrammar
	structure O: PICKLE_GRAMMAR = PickleGrammar

	val translate: C.t -> Source.desc * I.component -> O.t
    end
