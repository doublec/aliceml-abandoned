(*
 * Handling of internal inconsistencies.
 *)

structure Crash :> CRASH =
  struct
    exception Crash of string
  end
