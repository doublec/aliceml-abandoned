signature TARGET =
  sig
    structure C: CONTEXT
    structure Sig: SIGNATURE

    type t

    val sign: t -> Sig.t

    val apply: C.t -> t -> unit
    val save: C.t -> string -> t -> unit
  end
