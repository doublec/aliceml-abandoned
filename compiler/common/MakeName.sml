functor MakeName(Stamp: STAMP) :> NAME =
  struct

    type stamp    = int
    datatype exin = ExId of string | InId
    type name     = stamp * exin
    type t        = name

    fun new s		= ( Stamp.new(), ExId s )
    fun invent()	= ( Stamp.new(), InId )
    fun rename(_, exin)	= ( Stamp.new(), exin )

    fun toString(_, ExId s) = s
      | toString(n, InId)   = "$" ^ Int.toString n

    fun compare((n1,_), (n2,_)) = Int.compare(n1,n2)
    fun hash (n,_)              = n

  end
