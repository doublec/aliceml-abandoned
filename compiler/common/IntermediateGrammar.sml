structure IntermediateInfo =
  struct
    type t = Source.region * Type.t option
    exception Info
    fun region(r, to)  = r
    fun typ(r, SOME t) = t
      | typ(r, NONE)   = raise Info
  end

structure IntermediateGrammar =
	  MakeIntermediateGrammar(type info = IntermediateInfo.t
				  type sign = unit)
