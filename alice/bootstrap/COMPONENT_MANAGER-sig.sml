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

	val eval: Url.t * Component.t -> Reflect.module (* Component.Failure *)
	val load: Url.t -> Component.t                  (* IO.Io *)
	val link: Url.t -> Component.t                  (* Component.Failure *)
	val enter: Url.t * Component.t -> unit          (* Conflict *)
	val lookup: Url.t -> Component.t option
    end
