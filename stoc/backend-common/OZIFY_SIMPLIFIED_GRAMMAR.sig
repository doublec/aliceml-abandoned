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

signature OZIFYSIMPLIFIED =
    sig
	structure Simplified: SIMPLIFIED = Simplified
	val outputList:
	    (TextIO.outstream * 'a -> unit) ->
	    TextIO.outstream * 'a list -> unit
	val outputDec: TextIO.outstream * Simplified.dec -> unit
    end
