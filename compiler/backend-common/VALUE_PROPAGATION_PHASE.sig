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

signature VALUE_PROPAGATION_PHASE =
    sig
	structure C: CONTEXT = EmptyContext
	structure I: FLAT_GRAMMAR = FlatGrammar
	structure O: FLAT_GRAMMAR = FlatGrammar

	val translate: C.t -> I.t -> O.t
    end
