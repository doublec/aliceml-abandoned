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
from "TextIO"

signature UNIX_COMPONENT =
    sig
	signature UNIX =
	    sig
		type proc

		val execute: string * string list -> proc
		val streamsOf: proc -> TextIO.instream * TextIO.outstream
	    end

	structure Unix: UNIX
    end
