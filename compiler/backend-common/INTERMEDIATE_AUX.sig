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
	structure I: INTERMEDIATE_GRAMMAR = IntermediateGrammar
	structure O: IMPERATIVE_GRAMMAR = ImperativeGrammar

	val id_info: I.exp_info -> I.id_info
	val exp_info: Source.region * Type.t -> I.exp_info

	val freshId: I.exp_info -> I.id

	val idEq: I.id * I.id -> bool

	val occursInMatches: I.match list * I.id -> bool

	val patternVariablesOf: I.pat -> I.id list

	type subst = (I.id * I.id) list

	val substDec: I.dec * subst -> I.dec
	val substExp: I.exp * subst -> I.exp
	val substPat: I.pat * subst -> I.pat

	val separateAlt: I.pat -> I.pat

	val makeConArity: I.exp_info * bool -> O.conArity
    end
