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

signature IO_COMPONENT =
    sig
	structure IO:
	    sig
		exception Io of {name: string, function: string, cause: exn}
	    end
    end
