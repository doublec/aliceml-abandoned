signature TARGET =
  sig
    structure C: CONTEXT
    structure Sig: SIGNATURE

    type t

    val sign: t -> Sig.t

    val save: C.t -> string -> t -> unit
  end
