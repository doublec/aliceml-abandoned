(*
 * Handling of internal inconsistencies.
 *)


structure Crash :> CRASH =
  struct

    exception Crash of string

    fun crash message = raise Crash message

  end
