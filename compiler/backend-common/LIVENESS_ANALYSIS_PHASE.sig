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

signature LIVENESS_ANALYSIS_PHASE =
    sig
	structure I: FLAT_GRAMMAR = FlatGrammar

	val annotate: I.component -> unit
    end

