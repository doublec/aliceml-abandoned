(*
 * Authors:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 1999
 *   Leif Kornstaedt, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature OZIFY_IMPERATIVE_GRAMMAR =
    sig
	val outputProgram: TextIO.outstream * ImperativeGrammar.program -> unit
    end
