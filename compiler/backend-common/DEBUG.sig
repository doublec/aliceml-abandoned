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

signature DEBUG =
    sig
	structure Intermediate: INTERMEDIATE_GRAMMAR = IntermediateGrammar

	val seqToString: string -> ('a -> string) -> 'a list -> string
	val listToString: ('a -> string) -> 'a list -> string
	val setToString: ('a -> string) -> 'a list -> string
	val posToString: string list -> string
	val idToString: Intermediate.id -> string
	val longidToString: Intermediate.longid -> string
	val mappingToString: (string list * Intermediate.id) list -> string
	val substToString: (Intermediate.id * Intermediate.id) list -> string
	val litToString: Intermediate.lit -> string
	val labToString: Intermediate.lab -> string
	val patToString: Intermediate.pat -> string
    end
