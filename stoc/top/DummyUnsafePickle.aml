(* Dummy replacement for bootstrapping *)

signature PICKLE =
    sig
	val loadSign: Url.t -> Signature.t option
	val replaceSign: Url.t * Signature.t * string -> unit
    end

structure Pickle :> PICKLE =
    struct
	fun loadSign _ = NONE

	fun replaceSign (_, _, _) =
	    raise Crash.Crash
		"Pickle.replaceSign not available in bootstrap compiler"
    end
