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

signature LIVENESS_ANALYSIS_PHASE =
    sig
	structure I: IMPERATIVE_GRAMMAR = ImperativeGrammar

	val annotate: I.component -> unit
    end

