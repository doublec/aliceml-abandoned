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
	structure C: CONTEXT
	structure I: FLAT_GRAMMAR = FlatGrammar
	structure O: FLAT_GRAMMAR = FlatGrammar

	val translate: C.t -> Source.desc * I.t -> O.t
	val dumpContext: C.t -> PrettyPrint.doc
    end
