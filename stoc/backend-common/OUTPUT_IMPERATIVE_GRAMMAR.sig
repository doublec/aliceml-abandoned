(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature OUTPUT_IMPERATIVE_GRAMMAR =
    sig
	structure I: IMPERATIVE_GRAMMAR = ImperativeGrammar

	val outputProgram: I.body -> string
    end
