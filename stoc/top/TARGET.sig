signature TARGET =
  sig
    structure C: CONTEXT

    type t

    val apply: C.t -> t -> unit
    val save: C.t -> string -> t -> unit
  end
