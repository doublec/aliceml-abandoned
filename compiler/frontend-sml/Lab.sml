(*
 * Standard ML label identifiers
 *
 * Definition, section 2.4
 *)


structure Lab :> LAB =
  struct

    type Lab = string
    type t   = Lab

    fun fromString s   = s
    fun fromInt n      = Int.toString n
    fun fromLargeInt n = LargeInt.toString n
    fun toString s     = s

  end
