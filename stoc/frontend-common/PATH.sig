signature PATH =
  sig

    type stamp = Stamp.t
    type name  = Name.t
    type lab   = Lab.t

    datatype path = PLAIN of stamp * name | DOT of path * lab

    type t = path

    val compare :	path * path -> order
    val hash :		path -> int

  end
