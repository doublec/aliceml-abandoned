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
*)

structure Assert =
  struct

    exception Assert of string * int * int	(* (file, line, column) *)

    val failure = Assert("?", 0, 0)

    fun assert true  = ()
      | assert false = raise failure

  end
