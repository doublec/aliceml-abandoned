signature PHASE_ERROR =
  sig
    type error		(* a datatype of possible errors *)
    type warning	(* a datatype of possible warnings *)

    val error:	Error.region * error -> 'a	(* format and print error *)
    val warn:	Error.region * warning -> unit	(* format and print warning *)
  end
