(*
 * Authors:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 2001-2002
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Cell :> CELL =
struct
    structure Stamp		= MkStamp()

    type 'a cell		= Stamp.t * 'a ref
    type 'a t			= 'a cell

    fun cell x			= (Stamp.stamp(), ref x)
    fun content (_, ref x)	= x
    fun stamp (z, _)		= z
    fun replace((_,r), x)	= r := x

    fun equal((_,r1), (_,r2))	= r1 = r2
    fun hash (z,_)		= Stamp.hash z

    functor MkMap(type t)	= MkHashImpMap(type t    = t cell
					       val equal = equal
					       val hash  = hash)
end
