(*
 * Error handling.
 *)


structure Error :> ERROR =
  struct

    (* Import *)

    type position = Source.position


    (* Export *)

    exception Error of position * string

    fun print(pos, s) =
        TextIO.output(TextIO.stdErr,
		      Source.positionToString pos ^ ": " ^ s ^ "\n")

    fun error(pos, message) = ( print(pos,message) ; raise Error(pos,message) )
    fun warn (pos, message) =   print(pos, "warning: " ^ message)

  end
