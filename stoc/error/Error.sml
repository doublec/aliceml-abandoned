(*
 * Error handling.
 *)


structure Error :> ERROR =
  struct

    (* Import *)

    type position = Source.position


    (* Export *)

    exception ERROR of position * string

    fun error(pos, message) = raise ERROR(pos, message)

  end
