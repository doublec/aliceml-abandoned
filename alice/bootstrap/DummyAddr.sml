(*
 * Author:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 2001-2007
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Addr =
struct
    fun addr c = Cell.hash c
end
