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

signature PRIM_OPS =
    sig
	exception UnknownPrim

	val getArity: string -> Arity.t option   (* UnknownPrim *)
    end
