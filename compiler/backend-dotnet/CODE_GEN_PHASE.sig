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

signature CODE_GEN_PHASE =
    sig
	structure I: IMPERATIVE_GRAMMAR = ImperativeGrammar
	structure O: IL = IL

	val genComponent: I.component -> O.program
    end
