signature NAME =
  sig

    datatype name = ExId of string | InId
    type t        = name

    val compare :	name * name -> order
    val hash :		name -> int
    val toString :	name -> string

  end
