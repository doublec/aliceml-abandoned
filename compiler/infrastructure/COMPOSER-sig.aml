(* The Komponist *)

signature COMPOSER =
  sig
    structure Sig: SIGNATURE

    exception Corrupt

    val sign:	Url.t -> Sig.t		(* [Corrupt, IO.Io] *)
    val start:	Url.t -> unit		(* [Corrupt, IO.Io] *)
  end

signature COMPOSER' =
    sig
	structure Sig: SIGNATURE

	exception Corrupt

	val sign:	Url.t -> Sig.t		(* [Corrupt, IO.Io] *)
	val start:	Url.t -> unit		(* [Corrupt, IO.Io] *)

	val setAcquisitionMethod: (Url.t -> Sig.t) -> unit
    end
