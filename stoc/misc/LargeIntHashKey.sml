structure LargeIntHashKey : HASH_KEY =
  struct

    type t = LargeInt.int

    val equals = op=

    fun hash n =
	let
	    val n'  = abs n handle Overflow => 0
	in
	    LargeInt.toInt n' handle Overflow =>
		LargeInt.toInt(n' mod LargeInt.fromInt(valOf(Int.maxInt)))
			(* maxInt must exist if we get an overflow *)
	end
    
  end
