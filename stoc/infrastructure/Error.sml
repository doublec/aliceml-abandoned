(*
 * Error handling.
 *)


structure Error :> ERROR =
  struct

    (* Import *)

    type position = Source.position


    (* Export *)

    exception Error of position * string

    fun error(pos, message) = raise Error(pos, message)

  end
