(* Dummy replacement for bootstrapping *)

import
    structure Crash
from "../infrastructure/Crash"

signature PICKLE =
    sig
	val loadSign: string -> 'a option
	val replaceSign: string * 'a * string -> unit
    end

structure Pickle :> PICKLE =
    struct
	fun loadSign _ = NONE

	fun replaceSign (_, _, _) =
	    raise Crash.Crash
		"Pickle.replaceSign not available in bootstrap compiler"
    end
