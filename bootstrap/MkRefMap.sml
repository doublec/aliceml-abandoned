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

functor MkRefMap(structure Stamp : STAMP
		 type t  val stamp : t -> Stamp.t) : SIMPLE_IMP_MAP =
	MkHashImpMap(type t = t ref
		     val equals = op=
		     fun hash(ref x) = Stamp.hash(stamp x))
