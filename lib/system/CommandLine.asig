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

signature COMMAND_LINE_COMPONENT =
    sig
	signature COMMAND_LINE =
	    sig
		val name: unit -> string
		val arguments: unit -> string list
	    end

	structure CommandLine: COMMAND_LINE
    end
