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

    exception Corrupt

    val sign:	Url.t -> Sig.t		(* [Corrupt, IO.Io] *)
    val start:	Url.t -> unit		(* [Corrupt, IO.Io] *)
  end
