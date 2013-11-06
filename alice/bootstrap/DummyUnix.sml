(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000-2003
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature UNIX =
    sig
	type proc

	val execute: string * string list -> proc
	val streamsOf: proc -> TextIO.instream * TextIO.outstream
    end

structure Unix :> UNIX =
    struct
	type proc = TextIO.instream * TextIO.outstream

	fun execute (_, _) = (TextIO.stdIn, TextIO.stdOut)
	fun streamsOf (instream, outstream) = (instream, outstream)
    end
