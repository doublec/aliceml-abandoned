(*
 * Stamp generator.
 *)


functor MakeStamp() :> STAMP =
  struct

    type Stamp = int

    val r = ref 0

    fun reset()  =  r := 0
    fun new()    = (r := !r + 1; !r)

    val toString = Int.toString

  end
