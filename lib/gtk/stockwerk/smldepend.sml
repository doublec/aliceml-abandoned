(*
  smldepend
  
  Creates a dependency Makefile from a CM module list.
*)

fun depend (file, me) =
let
    fun makeTuple sep e nil     = e
      | makeTuple sep e [x]     = x
      | makeTuple sep e (x::xr) = x ^ sep ^ (makeTuple sep e xr)	

    val f = TextIO.openAppend file
in
    ( TextIO.output (f, me^": "^(makeTuple " \\\n " "" (CM.names()))^"\n") ;
     TextIO.closeOut f )
end
handle _ => ()
