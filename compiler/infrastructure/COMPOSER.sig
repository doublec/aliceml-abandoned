(* Abstract 1st class representation of signatures *)

signature SIGNATURE =
  sig
    type t
    val matches: t * t -> bool
  end

(* The Komponist *)

signature COMPOSER =
  sig
    structure Sig: SIGNATURE

    val sign:	Url.t -> Sig.t
    val start:	Url.t -> unit
  end
