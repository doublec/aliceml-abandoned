(*
 * A source file
 *)


structure Source :> SOURCE =
  struct

    type source   = string
    type pos      = int * int
    type position = pos * pos

    val nowhere = ((0,0),(0,0))

    fun over(pos1: position, pos2: position)	= (#1 pos1, #2 pos2)
    fun between(pos1: position, pos2: position)	= (#2 pos1, #1 pos2)

    fun posToString(lin,col) =
	Int.toString lin ^ "." ^ Int.toString col

    fun positionToString(position as (pos1,pos2)) =
	if position = nowhere then
	    "(unknown position)"
	else
	    posToString pos1 ^ "-" ^ posToString pos2

  end
