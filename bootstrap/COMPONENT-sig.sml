(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 2001-2003
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(* Dummy replacement for bootstrapping *)

signature COMPONENT =
    sig
	type component
	type t = component

	exception Sited
	exception Corrupt
	exception NotFound

	exception Mismatch of {component : Url.t,
			       request : Url.t option,
			       cause : Inf.mismatch}
	exception Eval of exn
	exception Failure of Url.t * exn

	val extension: string

	val defaultResolver: unit

	val load: Url.t -> component
	val save: string * component -> unit
	val inf: component -> Inf.t option

	functor MkManager() : (*COMPONENT_MANAGER*)
	    sig
		exception Conflict

		val eval: Url.t * component -> Reflect.module
		val link: Url.t -> component
		val enter: Url.t * component -> unit
		val lookup: Url.t -> component option
	    end
    end
