signature TARGET =
  sig
    structure C: CONTEXT

    type t

    val eval: C.t -> t -> C.t
    val save: string -> t -> unit
  end
