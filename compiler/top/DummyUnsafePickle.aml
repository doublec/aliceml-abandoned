(* Dummy replacement for bootstrapping *)

import
    structure Crash
from "../infrastructure/Crash"

signature UNSAFE_PICKLE =
    sig
	val loadSign: string -> 'a option
	val replaceSign: string * 'a * string -> unit
    end

structure UnsafePickle :> UNSAFE_PICKLE =
    struct
	fun loadSign _ = NONE

	fun replaceSign (_, _, _) =
	    raise Crash.Crash
		"UnsafePickle.replaceSign not available in bootstrap compiler"
    end
