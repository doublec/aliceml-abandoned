(*
 * Standard ML label identifiers
 *
 * Definition, section 2.4
 *)


functor Lab() :> LAB =
  struct

    type Lab = string

    fun fromString s = s
    val fromInt      = Int.toString
    fun toString s   = s

    fun compare(lab1, lab2) =
      case (Int.fromString lab1, Int.fromString lab2)
	of (SOME i1, SOME i2) => Int.compare(i1, i2)
	 |     _              => String.compare(lab1, lab2)

    fun equalsNum(lab, i) =
      case Int.fromString lab
	of SOME i' => i = i'
	 | NONE    => false

  end
