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

structure Future =
struct
    exception Future of exn

    fun innerExn(Future e) = innerExn e
      | innerExn e         = e
end
