(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature INTERMEDIATE_AUX =
    sig
	structure I: INTERMEDIATE_GRAMMAR = IntermediateGrammar

	val id_info: I.exp_info -> I.dec_info

	val freshIntermediateId: I.id_info -> I.id

	type subst = (Stamp.t * Stamp.t) list

	val substDec: I.dec * subst -> I.dec
	val substExp: I.exp * subst -> I.exp

	val separateAlt: I.pat -> I.pat

	val rowLabels: Type.row -> Label.t list
	val typToArity: Type.t -> Arity.t
	val makeConArity: Type.t * bool -> Arity.t option
	val labelToIndex: Type.t * Label.t -> int
    end
