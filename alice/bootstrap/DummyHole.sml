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

(* This is a glorious hack to fake holes for the Type module (mergeBind').
 * Note that it shadows the functions from the Cell structure!
 *
 * Since this dummy relies on Cell, it is included in lib/utility/sources.cm
 *)

structure Hole =
struct
    fun hole()		= ()
    fun replace(t,t')	= ()
    fun content t	= t
    fun fill(t,t')	= Cell.replace(t,t')
    fun isHole t	= false
end
