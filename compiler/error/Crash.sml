(*
 * Handling of internal inconsistencies.
 *)


structure Crash :> CRASH =
  struct

    exception CRASH of string

    fun crash(message) = raise CRASH(message)

  end
