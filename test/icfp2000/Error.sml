signature ERROR =
sig
    val error : int * string -> 'a
end

structure Error :> ERROR =
struct
    fun posToString ~1  = "(end of file)"
      | posToString pos = Int.toString pos

    fun error(pos, s) =
	( TextIO.output(TextIO.stdErr, posToString pos ^ ": " ^ s ^ "\n")
	; OS.Process.exit(OS.Process.failure)
	)
end
