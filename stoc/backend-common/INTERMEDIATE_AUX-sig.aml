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

signature INTERMEDIATE_AUX =
    sig
	structure Intermediate: INTERMEDIATE_GRAMMAR = IntermediateGrammar

	val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b

	val freshId: Intermediate.info -> Intermediate.id

	val patternVariablesOf: Intermediate.pat -> Intermediate.id list

	type subst = (Intermediate.id * Intermediate.id) list

	val substDec: Intermediate.dec * subst -> Intermediate.dec
	val substExp: Intermediate.exp * subst -> Intermediate.exp
	val substPat: Intermediate.pat * subst -> Intermediate.pat

	val separateAlt: Intermediate.pat -> Intermediate.pat
    end
