(*
 * Stamp generator.
 *)


functor Stamp() :> STAMP =
  struct

    type Stamp = int

    val r = ref 0

    fun reset()  =  r := 0
    fun fresh()  = (r := !r + 1; !r)

    val toString = Int.toString

  end
