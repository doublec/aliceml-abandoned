(* Dummy replacement for bootstrapping *)

signature PICKLE =
    sig
	val loadSign: Url.t -> Signature.t option
    end

structure Pickle :> PICKLE =
    struct
	fun loadSign _ = NONE
    end
