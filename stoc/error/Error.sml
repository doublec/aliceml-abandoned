(*
 * Error handling.
 *)


structure Error :> ERROR =
  struct

    (* Import *)

    type region = Source.region


    (* Export *)

    exception Error of region * string

    fun print(reg, s) =
        TextIO.output(TextIO.stdErr,
		      Source.regionToString reg ^ ": " ^ s ^ "\n")

    fun error(reg, message) = ( print(reg,message) ; raise Error(reg,message) )
    fun warn (reg, message) =   print(reg, "warning: " ^ message)

  end
