(*
 * Handling of internal inconsistencies.
 *)


structure Crash :> CRASH =
  struct

    exception Crash of string

    fun crash message =
	( TextIO.output(TextIO.stdOut, message ^ "\n")
	; raise Crash message
	)

  end
