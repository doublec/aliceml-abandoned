structure Fixity :> FIXITY =
  struct

    datatype assoc = LEFT | RIGHT | NEITHER	(* [a] *)

    datatype fix = NONFIX			(* [q] *)
		 | PREFIX  of int
		 | POSTFIX of int
		 | INFIX   of int * assoc

    type t = fix

  end
