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

signature INTERMEDIATEAUX =
    sig
	structure Intermediate: INTERMEDIATE = PostTranslationIntermediate

	val freshId: Intermediate.info -> Intermediate.id

	val idEq: Intermediate.id * Intermediate.id -> bool
	val longidEq: Intermediate.longid * Intermediate.longid -> bool

	val patternVariablesOf: Intermediate.pat -> Intermediate.id list

	type subst = (Intermediate.id * Intermediate.id) list

	val substDec: Intermediate.dec * subst -> Intermediate.dec
	val substExp: Intermediate.exp * subst -> Intermediate.exp
	val substPat: Intermediate.pat * subst -> Intermediate.pat

	val separateAlt: Intermediate.pat -> Intermediate.pat
    end
