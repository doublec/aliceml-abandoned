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
	structure O: FLAT_GRAMMAR = FlatGrammar

	val id_info: I.exp_info -> I.id_info
	val longid_info: I.exp_info -> I.longid_info

	val freshId: I.id_info -> I.id

	val idEq: I.id * I.id -> bool

	val occursInMatches: I.match list * I.id -> bool

	val patternVariablesOf: I.pat -> I.id list

	type subst = (I.id * I.id) list

	val substDec: I.dec * subst -> I.dec
	val substExp: I.exp * subst -> I.exp
	val substPat: I.pat * subst -> I.pat

	val separateAlt: I.pat -> I.pat

	val typToArity: Type.t -> O.arity
	val makeConArity: Type.t * bool -> O.conArity
	val findLabel: O.arity * Label.t -> int option
	val selIndex: Type.t * Label.t -> int
	val tagIndex: Type.t * Label.t -> int
    end
