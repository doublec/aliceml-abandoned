(* Dummy replacement for bootstrapping *)

structure UnsafeValue :> UNSAFE_VALUE =
    struct
	fun same (_, _) = false
    end
