(*
 * Stamp generator.
 *)


functor MakeStamp() : (*DEBUG :>*) STAMP =
  struct

    type stamp = int
    type t     = stamp

    val r = ref 0

    fun reset()  =  r := 0
    fun new()    = (r := !r + 1; !r)

    val toString = Int.toString
    val compare  = Int.compare

    fun hash n   = n

  end
