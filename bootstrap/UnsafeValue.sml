(* Dummy replacement for bootstrapping *)

structure UnsafeValue :> UNSAFE_VALUE =
    struct
	exception NotSupported

	val cast = Unsafe.cast
	fun same (x, y) = false
	fun awaitRequest x = x

	fun proj (prod, labels, i) = raise NotSupported
	fun projTuple (tuple, width, i) = raise NotSupported

	fun tag (sum, labels) = raise NotSupported
	fun projTagged (sum, labels, i) = raise NotSupported
	fun projTaggedTuple (sum, width, i) = raise NotSupported

	fun con ext = raise NotSupported
	fun projConstructed (ext, labels, i) = raise NotSupported
	fun projConstructedTuple (ext, width, i) = raise NotSupported

	fun projPoly (prod, label) = raise NotSupported

	fun prod labelValueVec = raise NotSupported
	fun tuple values = raise NotSupported

	fun tagged (labels, i, labelValueVec) = raise NotSupported
	fun taggedTuple (labels, i, values) = raise NotSupported

	fun closure (code, values) = raise NotSupported

	fun prim name = raise NotSupported

	fun conName con = raise NotSupported

	fun inArity function = ~2
	fun outArity function = ~2
    end
