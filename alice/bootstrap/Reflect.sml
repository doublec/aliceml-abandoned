(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 2001-2004
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(* Dummy replacement for bootstrapping *)

structure Reflect =
    struct
	type value  = unit
	type module = unit

	exception Reflect

	val reflect = Unsafe.cast : 'a -> value
	fun reflectPackage _ : 'a * 'b = raise Reflect
    end
