(*
 * Author:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Lock =
struct
    type lock		= unit

    fun lock()		= ()
    fun sync lock f x	= f x
end
