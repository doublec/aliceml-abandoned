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

signature PARTIAL_EVALUATION_PHASE =
    sig
	structure I: SIMPLIFIED_GRAMMAR = SimplifiedGrammar
	structure O: SIMPLIFIED_GRAMMAR = SimplifiedGrammar

	val main: I.program -> O.program
    end
