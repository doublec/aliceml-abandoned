(* This is a hack for SML/NJ until we are bootstrapped. Use
	Assert.assert(exp)
   for assertions and
	raise Assert.failure
   for definite failures.

   In Stockhausen assert will be a toplevel builtin (probably a keyword).
   Its type will be bool -> 'a (delivering a NoGood on return). Definite
   failures can then be written as
	assert false
   (very similar to O'Caml).

   Another design would be to have a derived form
	assert exp of pat => exp
   which is a derived form for
	case exp of pat => exp | _ => raise Assert(_)
   and an additional derived form
	assert exp => exp
   which expands to
	assert exp of true => exp
   (very much like if expands to case). But how could definite failure fit
   into this?
*)

structure Assert =
  struct

    exception Assert of string * int * int	(* (file, line, column) *)

    val failure = Assert("?", 0, 0)

    fun assert true  = ()
      | assert false = raise failure

  end
