(* Dummy replacement for bootstrapping *)

structure UnsafeValue =
    struct
	exception NotSupported

	val cast = Unsafe.cast
	fun same (x, y) = false
	val sameCode = same
	fun awaitRequest x = x

	fun realToVector r =
	    let
		val vec = Unsafe.blastWrite r
		val mkIndex =
		    case Word8Vector.sub (vec, 0) of
			0wx33 => (fn i => 103 - i)
		      | 0wx00 => (fn i => i + 96)
		      | _ => raise Match
	    in
		Word8Vector.tabulate
		    (8, fn i => Word8Vector.sub (vec, mkIndex i))
	    end

	fun proj _		= raise NotSupported
	fun projTuple _		= raise NotSupported
	fun tag _		= raise NotSupported
	fun projTagged _	= raise NotSupported
	fun projTaggedTuple _	= raise NotSupported
	fun con _		= raise NotSupported
	fun projConstructed _	= raise NotSupported
	fun projConstructedTuple _ = raise NotSupported
	fun projPoly _		= raise NotSupported
	fun prod _		= raise NotSupported
	fun tuple _		= raise NotSupported
	fun tagged _		= raise NotSupported
	fun taggedTuple _	= raise NotSupported
	fun conVal _		= raise NotSupported
	fun conValTuple _	= raise NotSupported
	fun closure _		= raise NotSupported
	fun prim _		= raise NotSupported
	fun conName _		= raise NotSupported

	fun inArity _		= ~2
	fun outArity _		= ~2
    end
