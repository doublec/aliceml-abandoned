(* Abstract 1st class representation of signatures *)

signature SIGNATURE =
  sig
    type t
    val matches: t * t -> bool
  end
