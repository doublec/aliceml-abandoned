(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 2001-2004
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(* Dummy replacement for bootstrapping *)

structure ComponentManager :> COMPONENT_MANAGER =
    struct
	open Component

	exception Conflict

	fun eval (url, _) = raise Failure (url, Eval NotFound)
	fun load url =
	    raise Failure (url, IO.Io {name = Url.toStringRaw url,
				       function = "load",
				       cause = Corrupt})
	fun link url =
	    raise Failure (url, IO.Io {name = Url.toStringRaw url,
				       function = "link",
				       cause = Corrupt})
	fun lookup _ = NONE
	fun enter (_, _) = raise Conflict
    end
