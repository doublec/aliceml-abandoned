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
    structure Url
from "../misc/Url"

import
    structure Inf
from "../common/Inf"

signature PICKLE_COMPONENT =
    sig
	structure Pickle:
	    sig
		val loadSign: Url.t -> Inf.sign option
		val replaceSign: Url.t * Inf.sign * string -> unit
	    end
    end
