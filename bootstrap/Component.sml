(* Dummy replacement for bootstrapping *)

structure Component :> COMPONENT =
    struct
	type component = unit
	type t = component

	exception Sited
	exception Corrupt

	fun inf _ = NONE
	fun load _ = raise Corrupt
	fun save (_, _) = raise Sited

	structure Manager: COMPONENT_MANAGER =
	    struct
		exception Conflict

		type component = component

		fun link _ = raise Corrupt
		fun lookup _ = NONE
		fun enter (_, _) = raise Conflict
	    end
    end

structure ComponentManager = Component.Manager
