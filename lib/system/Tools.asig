(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature TOOLS_COMPONENT =
    sig
	signature TOOLS =
	    sig
		val setPrintDepth: int -> unit
		val setPrintWidth: int -> unit

		val print: 'a -> unit
		val toString: 'a -> string
		val browse: 'a -> unit
		val inspect: 'a -> unit
	    end

	structure Tools: TOOLS
    end
