signature LAMBDA =
    sig
	type stamp = IntermediateGrammar.stamp
	type id = IntermediateGrammar.id

	val push           : id -> unit
	val pop            : unit -> unit
	val noSapply       : unit -> unit
	val sapplyPossible : unit -> bool
	val isStatic       : stamp -> bool
	val top            : unit -> stamp
	val pushFun    : id list -> unit
	val popFun     : unit -> unit
	val setId      : unit -> unit
	val getId      : stamp -> id
	val isSelfCall : stamp -> bool
	val getLambda  : stamp -> stamp
	val createIdsLambdaTabel : unit -> unit
	val assignName : (stamp * string) -> unit
	val getName    : stamp -> string
    end
