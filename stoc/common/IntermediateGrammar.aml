structure IntermediateInfo =
  struct
    type t = Source.region * Type.t option
    fun region(r,to) = r
    fun typ(r,to)    = to
  end

structure IntermediateGrammar =
	  MakeIntermediateGrammar(type info = IntermediateInfo.t
				  type sign = unit)
