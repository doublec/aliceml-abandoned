(* Dummy replacement for bootstrapping *)

signature COMPONENT_MANAGER =
    sig
	exception Conflict

	type component

	val link: Url.t -> component
	val enter: Url.t * component -> unit    (* Conflict *)
	val lookup: Url.t -> component option
    end
