(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000-2003
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(* Dummy replacement for bootstrapping *)

structure UnsafeComponent :> UNSAFE_COMPONENT =
    struct
	fun unavailable f =
	    (TextIO.output (TextIO.stdErr,
			    "UnsafeComponent." ^ f ^
			    "unavailable in bootstrap compiler");
	     assert false)

	fun load _ = unavailable "load"
	fun replaceSign (_, _) = unavailable "replaceSign"
	fun save (_, _) = unavailable "save"
    end
