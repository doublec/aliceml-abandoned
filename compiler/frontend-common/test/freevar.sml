datatype 'a t = T
fun f x = x
val _ = f T
(*
val g = #2
val x = g(2,3)
*)

fun f(x as {a,...}) =
    let
	structure S = struct val y = x end
    in
	()
    end
