(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature OUTPUT_FLAT_GRAMMAR =
    sig
	structure I: FLAT_GRAMMAR = FlatGrammar

	val outputComponent: I.component -> string
    end
