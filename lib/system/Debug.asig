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

signature DEBUG_COMPONENT =
    sig
	structure Debug:
	    sig
		val show: 'a -> unit

		structure Show: fct(S: any) -> sig end
	    end
    end
