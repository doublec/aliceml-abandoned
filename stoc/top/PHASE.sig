signature CONTEXT =
  sig
    type t
    val initial: unit -> t
    val clone:   t    -> t
  end

signature REPRESENTATION =
  sig
    type t
  end

signature PHASE =
  sig
    structure C: CONTEXT
    structure I: REPRESENTATION
    structure O: REPRESENTATION

    val translate: C.t -> I.t -> O.t   (* [Error.Error] *)
  end

signature PHASE_ERROR =
  sig
    type error		(* a datatype of possible errors *)
    type warning	(* a datatype of possible warnings *)

    val error:	Error.region * error -> 'a	(* format and print error *)
    val warn:	Error.region * warning -> unit	(* format and print warning *)
  end
