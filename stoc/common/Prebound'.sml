structure Prebound' :> PREBOUND' =
  struct

    type name   = IntermediateGrammar.stamp

    open IntermediateGrammar

    val dummy = Source.nowhere

    val stamp_builtin	= Stamp.new()

  end
