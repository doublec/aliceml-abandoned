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

signature MATCH_COMPILATION_PHASE =
    sig
	structure I: INTERMEDIATE_GRAMMAR = IntermediateGrammar
	structure O: SIMPLIFIED_GRAMMAR = SimplifiedGrammar

	val simplify: I.program -> O.program
    end
