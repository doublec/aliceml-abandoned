(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature BATCH_COMPILER =
    sig
	val stoc: string list -> OS.Process.status
    end
