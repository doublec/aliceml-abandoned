(* Dummy replacement for bootstrapping *)

structure Reflect =
    struct
	type value = unit

	fun realToVector r =
	    let
		val vec = Unsafe.blastWrite r
		val mkIndex =
		    case Word8Vector.sub (vec, 0) of
			0wx33 => (fn i => 103 - i)
		      | 0wx00 => (fn i => i + 96)
		      | _ => raise Match
	    in
		Vector.tabulate
		    (8, fn i => Word8Vector.sub (vec, mkIndex i))
	    end
    end
