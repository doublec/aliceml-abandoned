(* Dummy replacement for bootstrapping *)

signature COMPONENT =
    sig
	type component
	type t = component

	exception Sited
	exception Corrupt

	val load: Url.t -> component
	val save: string * component -> unit
	val inf: component -> Inf.t option

	structure Manager: COMPONENT_MANAGER where type component = component
    end
