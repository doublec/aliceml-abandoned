(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2001-2003
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(* Dummy replacement for bootstrapping *)

signature COMPONENT_MANAGER =
    sig
	exception Conflict

	type component

	val eval: Url.t * component -> Reflect.module (* Component.Failure *)
	val link: Url.t -> component                  (* Component.Failure *)
	val enter: Url.t * component -> unit          (* Conflict *)
	val lookup: Url.t -> component option
    end
