(*
 * Author:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 2001-2002
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Future =
struct
    datatype status = FUTURE | FAILED | DETERMINED
    exception Cyclic

    fun concur f	= f()
    fun byneed f	= f()

    fun await x		= x
    fun awaitEither(x,_)= x

    fun status x	= DETERMINED
    fun isFuture x	= false
    fun isFailed x	= false
    fun isDetermined x	= true
    fun isLazy x	= false
end
