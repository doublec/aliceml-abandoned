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

signature TEXT_IO_COMPONENT =
    sig
	signature TEXT_IO =
	    sig
		type instream
		type outstream

		type vector = string
		type elem = char

		val stdIn : instream
		val openIn : string -> instream
		val inputAll : instream -> vector
		val inputLine : instream -> vector
		val closeIn : instream -> unit
		val stdOut : outstream
		val stdErr : outstream
		val openOut : string -> outstream
		val output : outstream * vector -> unit
		val output1 : outstream * elem -> unit
		val flushOut : outstream -> unit
		val closeOut : outstream -> unit
		val print : string -> unit
	    end

	structure TextIO: TEXT_IO

	val print : string -> unit
    end
