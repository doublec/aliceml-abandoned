(* -*- sml -*- *)

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

import
    structure TextIO
from "lib/TextIO.ozf"

signature UNIX_COMPONENT =
    sig
	structure Unix:
	    sig
		type proc

		val execute: string * string list -> proc
		val streamsOf: proc -> TextIO.instream * TextIO.outstream
	    end
    end
