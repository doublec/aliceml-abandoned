(*
 * Authors:
 *   Sven Woop <woop@ps.uni-sb.de>
 *
 * Copyright:
 *   Sven Woop, 2003
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

signature PATTERNMATCHING =
    sig
	type pattern
	type assignment

	val createPattern : string -> pattern
	val matchPattern : string * pattern -> assignment option
	val substPattern : pattern * assignment -> string
	val printPattern : pattern -> unit
    end
