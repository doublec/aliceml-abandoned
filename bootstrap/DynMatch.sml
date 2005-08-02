(*
 * Authors:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 2004
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(* Dummy replacement for bootstrapping *)

structure DynMatch =
struct
    type module		= unit
    exception MatchDummy
    fun dummy _		= raise MatchDummy
    val match		= dummy
    val matchWith	= dummy
    val strengthen	= dummy
    val strengthenWith	= dummy
    val thin		= dummy
    val seal		= dummy
    val unpackMatch	= dummy
end
